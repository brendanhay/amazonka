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

-- Module      : Network.AWS.OpsWorks.DescribeStackProvisioningParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Requests a description of a stack's provisioning parameters.
--
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeStackProvisioningParameters.html>
module Network.AWS.OpsWorks.DescribeStackProvisioningParameters
    (
    -- * Request
      DescribeStackProvisioningParameters
    -- ** Request constructor
    , describeStackProvisioningParameters
    -- ** Request lenses
    , dsppStackId

    -- * Response
    , DescribeStackProvisioningParametersResponse
    -- ** Response constructor
    , describeStackProvisioningParametersResponse
    -- ** Response lenses
    , dspprAgentInstallerUrl
    , dspprParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DescribeStackProvisioningParameters = DescribeStackProvisioningParameters
    { _dsppStackId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DescribeStackProvisioningParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsppStackId' @::@ 'Text'
--
describeStackProvisioningParameters :: Text -- ^ 'dsppStackId'
                                    -> DescribeStackProvisioningParameters
describeStackProvisioningParameters p1 = DescribeStackProvisioningParameters
    { _dsppStackId = p1
    }

-- | The stack ID
dsppStackId :: Lens' DescribeStackProvisioningParameters Text
dsppStackId = lens _dsppStackId (\s a -> s { _dsppStackId = a })

data DescribeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse
    { _dspprAgentInstallerUrl :: Maybe Text
    , _dspprParameters        :: Map Text Text
    } deriving (Eq, Show)

-- | 'DescribeStackProvisioningParametersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dspprAgentInstallerUrl' @::@ 'Maybe' 'Text'
--
-- * 'dspprParameters' @::@ 'HashMap' 'Text' 'Text'
--
describeStackProvisioningParametersResponse :: DescribeStackProvisioningParametersResponse
describeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse
    { _dspprAgentInstallerUrl = Nothing
    , _dspprParameters        = mempty
    }

-- | The AWS OpsWorks agent installer's URL.
dspprAgentInstallerUrl :: Lens' DescribeStackProvisioningParametersResponse (Maybe Text)
dspprAgentInstallerUrl =
    lens _dspprAgentInstallerUrl (\s a -> s { _dspprAgentInstallerUrl = a })

-- | An embedded object that contains the provisioning parameters.
dspprParameters :: Lens' DescribeStackProvisioningParametersResponse (HashMap Text Text)
dspprParameters = lens _dspprParameters (\s a -> s { _dspprParameters = a }) . _Map

instance ToPath DescribeStackProvisioningParameters where
    toPath = const "/"

instance ToQuery DescribeStackProvisioningParameters where
    toQuery = const mempty

instance ToHeaders DescribeStackProvisioningParameters

instance ToJSON DescribeStackProvisioningParameters where
    toJSON DescribeStackProvisioningParameters{..} = object
        [ "StackId" .= _dsppStackId
        ]

instance AWSRequest DescribeStackProvisioningParameters where
    type Sv DescribeStackProvisioningParameters = OpsWorks
    type Rs DescribeStackProvisioningParameters = DescribeStackProvisioningParametersResponse

    request  = post "DescribeStackProvisioningParameters"
    response = jsonResponse

instance FromJSON DescribeStackProvisioningParametersResponse where
    parseJSON = withObject "DescribeStackProvisioningParametersResponse" $ \o -> DescribeStackProvisioningParametersResponse
        <$> o .:? "AgentInstallerUrl"
        <*> o .:? "Parameters" .!= mempty
