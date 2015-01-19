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

-- Module      : Network.AWS.RDS.ApplyPendingMaintenanceAction
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

-- | Applies a pending maintenance action to a resource.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ApplyPendingMaintenanceAction.html>
module Network.AWS.RDS.ApplyPendingMaintenanceAction
    (
    -- * Request
      ApplyPendingMaintenanceAction
    -- ** Request constructor
    , applyPendingMaintenanceAction
    -- ** Request lenses
    , apmaApplyAction
    , apmaOptInType
    , apmaResourceIdentifier

    -- * Response
    , ApplyPendingMaintenanceActionResponse
    -- ** Response constructor
    , applyPendingMaintenanceActionResponse
    -- ** Response lenses
    , apmarResourcePendingMaintenanceActions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction
    { _apmaApplyAction        :: Text
    , _apmaOptInType          :: Text
    , _apmaResourceIdentifier :: Text
    } deriving (Eq, Ord, Show)

-- | 'ApplyPendingMaintenanceAction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apmaApplyAction' @::@ 'Text'
--
-- * 'apmaOptInType' @::@ 'Text'
--
-- * 'apmaResourceIdentifier' @::@ 'Text'
--
applyPendingMaintenanceAction :: Text -- ^ 'apmaResourceIdentifier'
                              -> Text -- ^ 'apmaApplyAction'
                              -> Text -- ^ 'apmaOptInType'
                              -> ApplyPendingMaintenanceAction
applyPendingMaintenanceAction p1 p2 p3 = ApplyPendingMaintenanceAction
    { _apmaResourceIdentifier = p1
    , _apmaApplyAction        = p2
    , _apmaOptInType          = p3
    }

-- | The pending maintenance action to apply to this resource.
apmaApplyAction :: Lens' ApplyPendingMaintenanceAction Text
apmaApplyAction = lens _apmaApplyAction (\s a -> s { _apmaApplyAction = a })

-- | Specify an opt-in request, or undo an opt-in request. An opt-in request of
-- type 'immediate' cannot be undone.
--
-- Valid values:
--
-- 'immediate' - Apply the maintenance action immediately.  'next-maintenance' -
-- Apply the maintenance action during the next maintenance window for the
-- resource.  'undo-opt-in' - Cancel any existing 'next-maintenance' opt-in requests.
--
apmaOptInType :: Lens' ApplyPendingMaintenanceAction Text
apmaOptInType = lens _apmaOptInType (\s a -> s { _apmaOptInType = a })

-- | The ARN of the resource (for example, a DB Instance) that the pending
-- maintenance action applies to.
apmaResourceIdentifier :: Lens' ApplyPendingMaintenanceAction Text
apmaResourceIdentifier =
    lens _apmaResourceIdentifier (\s a -> s { _apmaResourceIdentifier = a })

newtype ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse
    { _apmarResourcePendingMaintenanceActions :: Maybe ResourcePendingMaintenanceActions
    } deriving (Eq, Show)

-- | 'ApplyPendingMaintenanceActionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apmarResourcePendingMaintenanceActions' @::@ 'Maybe' 'ResourcePendingMaintenanceActions'
--
applyPendingMaintenanceActionResponse :: ApplyPendingMaintenanceActionResponse
applyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse
    { _apmarResourcePendingMaintenanceActions = Nothing
    }

apmarResourcePendingMaintenanceActions :: Lens' ApplyPendingMaintenanceActionResponse (Maybe ResourcePendingMaintenanceActions)
apmarResourcePendingMaintenanceActions =
    lens _apmarResourcePendingMaintenanceActions
        (\s a -> s { _apmarResourcePendingMaintenanceActions = a })

instance ToPath ApplyPendingMaintenanceAction where
    toPath = const "/"

instance ToQuery ApplyPendingMaintenanceAction where
    toQuery ApplyPendingMaintenanceAction{..} = mconcat
        [ "ApplyAction"        =? _apmaApplyAction
        , "OptInType"          =? _apmaOptInType
        , "ResourceIdentifier" =? _apmaResourceIdentifier
        ]

instance ToHeaders ApplyPendingMaintenanceAction

instance AWSRequest ApplyPendingMaintenanceAction where
    type Sv ApplyPendingMaintenanceAction = RDS
    type Rs ApplyPendingMaintenanceAction = ApplyPendingMaintenanceActionResponse

    request  = post "ApplyPendingMaintenanceAction"
    response = xmlResponse

instance FromXML ApplyPendingMaintenanceActionResponse where
    parseXML = withElement "ApplyPendingMaintenanceActionResult" $ \x -> ApplyPendingMaintenanceActionResponse
        <$> x .@? "ResourcePendingMaintenanceActions"
