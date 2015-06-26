{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Endpoint where

import qualified Data.ByteString             as BS
import qualified Data.CaseInsensitive        as CI
import qualified Data.HashSet                as Set
import           Data.Monoid
import           Network.AWS.Data.ByteString
import           Network.AWS.Types

-- | Determine the full host address and credential scope for a 'Service'
-- within the specified 'Region'.
defaultEndpoint :: Service s -> Region -> Endpoint
defaultEndpoint Service{..} r = go (CI.mk _svcPrefix)
  where
    go = \case
        "iam"
            | china     -> region "iam.cn-north-1.amazonaws.com.cn"
            | govcloud  -> region "iam.us-gov.amazonaws.com"
            | otherwise -> global "iam.amazonaws.com"

        "sdb"
            | virginia  -> region "sdb.amazonaws.com"

        "sts"
            | china     -> region "sts.cn-north-1.amazonaws.com.cn"
            | govcloud  -> region ("sts." <> reg <> ".amazonaws.com")
            | otherwise -> global "sts.amazonaws.com"

        "s3"
            | virginia  -> global "s3.amazonaws.com"
            | china     -> region ("s3." <> reg <> ".amazonaws.com.cn")
            | s3        -> region ("s3-" <> reg <> ".amazonaws.com")

        "rds"
            | virginia  -> global "rds.amazonaws.com"

        "route53"
            | not china -> global "route53.amazonaws.com"

        "emr"
            | virginia  -> global "elasticmapreduce.us-east-1.amazonaws.com"
            | otherwise -> region (reg <> ".elasticmapreduce.amazonaws.com")

        "sqs"
            | virginia  -> global "queue.amazonaws.com"
            | china     -> region (reg <> ".queue.amazonaws.com.cn")

        "importexport"
            | not china -> region "importexport.amazonaws.com"

        "cloudfront"
            | not china -> global "cloudfront.amazonaws.com"

        _   | china     -> region (_svcPrefix <> "." <> reg <> ".amazonaws.com.cn")
            | otherwise -> region (_svcPrefix <> "." <> reg <> ".amazonaws.com")

    virginia = r == NorthVirginia

    s3 = r `Set.member` except

    govcloud = "us-gov" `BS.isPrefixOf` reg
    china    = "cn-"    `BS.isPrefixOf` reg

    region h = Endpoint { _endpointHost = h, _endpointScope = reg }
    global h = Endpoint { _endpointHost = h, _endpointScope = "us-east-1" }

    reg = toBS r

    except = Set.fromList
        [ GovCloud
        , GovCloudFIPS
        , Ireland
        , NorthCalifornia
        , NorthVirginia
        , Oregon
        , SaoPaulo
        , Singapore
        , Sydney
        , Tokyo
        ]
