{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateMaintenanceWindow
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Maintenance Window.
--
--
module Network.AWS.SSM.CreateMaintenanceWindow
    (
    -- * Creating a Request
      createMaintenanceWindow
    , CreateMaintenanceWindow
    -- * Request Lenses
    , cmwClientToken
    , cmwDescription
    , cmwName
    , cmwSchedule
    , cmwDuration
    , cmwCutoff
    , cmwAllowUnassociatedTargets

    -- * Destructuring the Response
    , createMaintenanceWindowResponse
    , CreateMaintenanceWindowResponse
    -- * Response Lenses
    , cmwrsWindowId
    , cmwrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'createMaintenanceWindow' smart constructor.
data CreateMaintenanceWindow = CreateMaintenanceWindow'
    { _cmwClientToken              :: !(Maybe Text)
    , _cmwDescription              :: !(Maybe (Sensitive Text))
    , _cmwName                     :: !Text
    , _cmwSchedule                 :: !Text
    , _cmwDuration                 :: !Nat
    , _cmwCutoff                   :: !Nat
    , _cmwAllowUnassociatedTargets :: !Bool
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmwClientToken' - User-provided idempotency token.
--
-- * 'cmwDescription' - An optional description for the Maintenance Window. We recommend specifying a description to help your organize your Maintenance Windows.
--
-- * 'cmwName' - The name of the Maintenance Window.
--
-- * 'cmwSchedule' - The schedule of the Maintenance Window in the form of a cron or rate expression.
--
-- * 'cmwDuration' - The duration of the Maintenance Window in hours.
--
-- * 'cmwCutoff' - The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
--
-- * 'cmwAllowUnassociatedTargets' - Whether targets must be registered with the Maintenance Window before tasks can be defined for those targets.
createMaintenanceWindow
    :: Text -- ^ 'cmwName'
    -> Text -- ^ 'cmwSchedule'
    -> Natural -- ^ 'cmwDuration'
    -> Natural -- ^ 'cmwCutoff'
    -> Bool -- ^ 'cmwAllowUnassociatedTargets'
    -> CreateMaintenanceWindow
createMaintenanceWindow pName_ pSchedule_ pDuration_ pCutoff_ pAllowUnassociatedTargets_ =
    CreateMaintenanceWindow'
    { _cmwClientToken = Nothing
    , _cmwDescription = Nothing
    , _cmwName = pName_
    , _cmwSchedule = pSchedule_
    , _cmwDuration = _Nat # pDuration_
    , _cmwCutoff = _Nat # pCutoff_
    , _cmwAllowUnassociatedTargets = pAllowUnassociatedTargets_
    }

-- | User-provided idempotency token.
cmwClientToken :: Lens' CreateMaintenanceWindow (Maybe Text)
cmwClientToken = lens _cmwClientToken (\ s a -> s{_cmwClientToken = a});

-- | An optional description for the Maintenance Window. We recommend specifying a description to help your organize your Maintenance Windows.
cmwDescription :: Lens' CreateMaintenanceWindow (Maybe Text)
cmwDescription = lens _cmwDescription (\ s a -> s{_cmwDescription = a}) . mapping _Sensitive;

-- | The name of the Maintenance Window.
cmwName :: Lens' CreateMaintenanceWindow Text
cmwName = lens _cmwName (\ s a -> s{_cmwName = a});

-- | The schedule of the Maintenance Window in the form of a cron or rate expression.
cmwSchedule :: Lens' CreateMaintenanceWindow Text
cmwSchedule = lens _cmwSchedule (\ s a -> s{_cmwSchedule = a});

-- | The duration of the Maintenance Window in hours.
cmwDuration :: Lens' CreateMaintenanceWindow Natural
cmwDuration = lens _cmwDuration (\ s a -> s{_cmwDuration = a}) . _Nat;

-- | The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
cmwCutoff :: Lens' CreateMaintenanceWindow Natural
cmwCutoff = lens _cmwCutoff (\ s a -> s{_cmwCutoff = a}) . _Nat;

-- | Whether targets must be registered with the Maintenance Window before tasks can be defined for those targets.
cmwAllowUnassociatedTargets :: Lens' CreateMaintenanceWindow Bool
cmwAllowUnassociatedTargets = lens _cmwAllowUnassociatedTargets (\ s a -> s{_cmwAllowUnassociatedTargets = a});

instance AWSRequest CreateMaintenanceWindow where
        type Rs CreateMaintenanceWindow =
             CreateMaintenanceWindowResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreateMaintenanceWindowResponse' <$>
                   (x .?> "WindowId") <*> (pure (fromEnum s)))

instance Hashable CreateMaintenanceWindow

instance NFData CreateMaintenanceWindow

instance ToHeaders CreateMaintenanceWindow where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreateMaintenanceWindow" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateMaintenanceWindow where
        toJSON CreateMaintenanceWindow'{..}
          = object
              (catMaybes
                 [("ClientToken" .=) <$> _cmwClientToken,
                  ("Description" .=) <$> _cmwDescription,
                  Just ("Name" .= _cmwName),
                  Just ("Schedule" .= _cmwSchedule),
                  Just ("Duration" .= _cmwDuration),
                  Just ("Cutoff" .= _cmwCutoff),
                  Just
                    ("AllowUnassociatedTargets" .=
                       _cmwAllowUnassociatedTargets)])

instance ToPath CreateMaintenanceWindow where
        toPath = const "/"

instance ToQuery CreateMaintenanceWindow where
        toQuery = const mempty

-- | /See:/ 'createMaintenanceWindowResponse' smart constructor.
data CreateMaintenanceWindowResponse = CreateMaintenanceWindowResponse'
    { _cmwrsWindowId       :: !(Maybe Text)
    , _cmwrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmwrsWindowId' - The ID of the created Maintenance Window.
--
-- * 'cmwrsResponseStatus' - -- | The response status code.
createMaintenanceWindowResponse
    :: Int -- ^ 'cmwrsResponseStatus'
    -> CreateMaintenanceWindowResponse
createMaintenanceWindowResponse pResponseStatus_ =
    CreateMaintenanceWindowResponse'
    { _cmwrsWindowId = Nothing
    , _cmwrsResponseStatus = pResponseStatus_
    }

-- | The ID of the created Maintenance Window.
cmwrsWindowId :: Lens' CreateMaintenanceWindowResponse (Maybe Text)
cmwrsWindowId = lens _cmwrsWindowId (\ s a -> s{_cmwrsWindowId = a});

-- | -- | The response status code.
cmwrsResponseStatus :: Lens' CreateMaintenanceWindowResponse Int
cmwrsResponseStatus = lens _cmwrsResponseStatus (\ s a -> s{_cmwrsResponseStatus = a});

instance NFData CreateMaintenanceWindowResponse
