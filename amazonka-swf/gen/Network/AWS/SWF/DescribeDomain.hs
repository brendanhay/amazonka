{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DescribeDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified domain including description and
-- status.
--
-- Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. You cannot use an IAM policy to constrain this action's
-- parameters.  If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails by throwing 'OperationNotPermitted'. For details and example IAM
-- policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DescribeDomain.html>
module Network.AWS.SWF.DescribeDomain
    (
    -- * Request
      DescribeDomain
    -- ** Request constructor
    , describeDomain
    -- ** Request lenses
    , ddName

    -- * Response
    , DescribeDomainResponse
    -- ** Response constructor
    , describeDomainResponse
    -- ** Response lenses
    , ddrConfiguration
    , ddrDomainInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

newtype DescribeDomain = DescribeDomain
    { _ddName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DescribeDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddName' @::@ 'Text'
--
describeDomain :: Text -- ^ 'ddName'
               -> DescribeDomain
describeDomain p1 = DescribeDomain
    { _ddName = p1
    }

-- | The name of the domain to describe.
--
ddName :: Lens' DescribeDomain Text
ddName = lens _ddName (\s a -> s { _ddName = a })

data DescribeDomainResponse = DescribeDomainResponse
    { _ddrConfiguration :: DomainConfiguration
    , _ddrDomainInfo    :: DomainInfo
    } deriving (Eq, Show)

-- | 'DescribeDomainResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrConfiguration' @::@ 'DomainConfiguration'
--
-- * 'ddrDomainInfo' @::@ 'DomainInfo'
--
describeDomainResponse :: DomainInfo -- ^ 'ddrDomainInfo'
                       -> DomainConfiguration -- ^ 'ddrConfiguration'
                       -> DescribeDomainResponse
describeDomainResponse p1 p2 = DescribeDomainResponse
    { _ddrDomainInfo    = p1
    , _ddrConfiguration = p2
    }

ddrConfiguration :: Lens' DescribeDomainResponse DomainConfiguration
ddrConfiguration = lens _ddrConfiguration (\s a -> s { _ddrConfiguration = a })

ddrDomainInfo :: Lens' DescribeDomainResponse DomainInfo
ddrDomainInfo = lens _ddrDomainInfo (\s a -> s { _ddrDomainInfo = a })

instance ToPath DescribeDomain where
    toPath = const "/"

instance ToQuery DescribeDomain where
    toQuery = const mempty

instance ToHeaders DescribeDomain

instance ToJSON DescribeDomain where
    toJSON DescribeDomain{..} = object
        [ "name" .= _ddName
        ]

instance AWSRequest DescribeDomain where
    type Sv DescribeDomain = SWF
    type Rs DescribeDomain = DescribeDomainResponse

    request  = post "DescribeDomain"
    response = jsonResponse

instance FromJSON DescribeDomainResponse where
    parseJSON = withObject "DescribeDomainResponse" $ \o -> DescribeDomainResponse
        <$> o .:  "configuration"
        <*> o .:  "domainInfo"
