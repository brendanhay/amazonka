{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.ResetClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets one or more parameters of the specified parameter group to their
-- default values and sets the source values of the parameters to
-- "engine-default". To reset the entire parameter group specify the
-- ResetAllParameters parameter. For parameter changes to take effect you must
-- reboot any associated clusters. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ResetClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Parameters.member.1.ParameterName=extra_float_digits &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T020847Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Your parameter group has
-- been updated but changes won't get applied until you reboot the associated
-- Clusters. parametergroup1 625d23c1-40dc-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift.V2012_12_01.ResetClusterParameterGroup where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.Redshift.V2012_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ResetClusterParameterGroup' request.
resetClusterParameterGroup :: Text -- ^ '_rcpgmParameterGroupName'
                           -> ResetClusterParameterGroup
resetClusterParameterGroup p1 = ResetClusterParameterGroup
    { _rcpgmParameterGroupName = p1
    , _rcpgmResetAllParameters = Nothing
    , _rcpgmParameters = mempty
    }

data ResetClusterParameterGroup = ResetClusterParameterGroup
    { _rcpgmParameterGroupName :: Text
      -- ^ The name of the cluster parameter group to be reset.
    , _rcpgmResetAllParameters :: Maybe Bool
      -- ^ If true, all parameters in the specified parameter group will be
      -- reset to their default values. Default: true.
    , _rcpgmParameters :: [Parameter]
      -- ^ An array of names of parameters to be reset. If
      -- ResetAllParameters option is not used, then at least one
      -- parameter name must be supplied. Constraints: A maximum of 20
      -- parameters can be reset in a single request.
    } deriving (Generic)

instance ToQuery ResetClusterParameterGroup where
    toQuery = genericToQuery def

instance AWSRequest ResetClusterParameterGroup where
    type Sv ResetClusterParameterGroup = Redshift
    type Rs ResetClusterParameterGroup = ResetClusterParameterGroupResponse

    request = post "ResetClusterParameterGroup"
    response _ = xmlResponse

data ResetClusterParameterGroupResponse = ResetClusterParameterGroupResponse
    { _cpgnmParameterGroupStatus :: Maybe Text
      -- ^ The status of the parameter group. For example, if you made a
      -- change to a parameter group name-value pair, then the change
      -- could be pending a reboot of an associated cluster.
    , _cpgnmParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    } deriving (Generic)

instance FromXML ResetClusterParameterGroupResponse where
    fromXMLOptions = xmlOptions
