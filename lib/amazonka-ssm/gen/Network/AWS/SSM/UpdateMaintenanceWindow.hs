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
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Maintenance Window. Only specified parameters are modified.
--
--
module Network.AWS.SSM.UpdateMaintenanceWindow
    (
    -- * Creating a Request
      updateMaintenanceWindow
    , UpdateMaintenanceWindow
    -- * Request Lenses
    , umwReplace
    , umwEnabled
    , umwSchedule
    , umwName
    , umwCutoff
    , umwAllowUnassociatedTargets
    , umwDescription
    , umwDuration
    , umwWindowId

    -- * Destructuring the Response
    , updateMaintenanceWindowResponse
    , UpdateMaintenanceWindowResponse
    -- * Response Lenses
    , umwrsEnabled
    , umwrsSchedule
    , umwrsName
    , umwrsCutoff
    , umwrsAllowUnassociatedTargets
    , umwrsDescription
    , umwrsDuration
    , umwrsWindowId
    , umwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'updateMaintenanceWindow' smart constructor.
data UpdateMaintenanceWindow = UpdateMaintenanceWindow'
  { _umwReplace                  :: !(Maybe Bool)
  , _umwEnabled                  :: !(Maybe Bool)
  , _umwSchedule                 :: !(Maybe Text)
  , _umwName                     :: !(Maybe Text)
  , _umwCutoff                   :: !(Maybe Nat)
  , _umwAllowUnassociatedTargets :: !(Maybe Bool)
  , _umwDescription              :: !(Maybe (Sensitive Text))
  , _umwDuration                 :: !(Maybe Nat)
  , _umwWindowId                 :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umwReplace' - If True, then all fields that are required by the CreateMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
--
-- * 'umwEnabled' - Whether the Maintenance Window is enabled.
--
-- * 'umwSchedule' - The schedule of the Maintenance Window in the form of a cron or rate expression.
--
-- * 'umwName' - The name of the Maintenance Window.
--
-- * 'umwCutoff' - The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
--
-- * 'umwAllowUnassociatedTargets' - Whether targets must be registered with the Maintenance Window before tasks can be defined for those targets.
--
-- * 'umwDescription' - An optional description for the update request.
--
-- * 'umwDuration' - The duration of the Maintenance Window in hours.
--
-- * 'umwWindowId' - The ID of the Maintenance Window to update.
updateMaintenanceWindow
    :: Text -- ^ 'umwWindowId'
    -> UpdateMaintenanceWindow
updateMaintenanceWindow pWindowId_ =
  UpdateMaintenanceWindow'
    { _umwReplace = Nothing
    , _umwEnabled = Nothing
    , _umwSchedule = Nothing
    , _umwName = Nothing
    , _umwCutoff = Nothing
    , _umwAllowUnassociatedTargets = Nothing
    , _umwDescription = Nothing
    , _umwDuration = Nothing
    , _umwWindowId = pWindowId_
    }


-- | If True, then all fields that are required by the CreateMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
umwReplace :: Lens' UpdateMaintenanceWindow (Maybe Bool)
umwReplace = lens _umwReplace (\ s a -> s{_umwReplace = a})

-- | Whether the Maintenance Window is enabled.
umwEnabled :: Lens' UpdateMaintenanceWindow (Maybe Bool)
umwEnabled = lens _umwEnabled (\ s a -> s{_umwEnabled = a})

-- | The schedule of the Maintenance Window in the form of a cron or rate expression.
umwSchedule :: Lens' UpdateMaintenanceWindow (Maybe Text)
umwSchedule = lens _umwSchedule (\ s a -> s{_umwSchedule = a})

-- | The name of the Maintenance Window.
umwName :: Lens' UpdateMaintenanceWindow (Maybe Text)
umwName = lens _umwName (\ s a -> s{_umwName = a})

-- | The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
umwCutoff :: Lens' UpdateMaintenanceWindow (Maybe Natural)
umwCutoff = lens _umwCutoff (\ s a -> s{_umwCutoff = a}) . mapping _Nat

-- | Whether targets must be registered with the Maintenance Window before tasks can be defined for those targets.
umwAllowUnassociatedTargets :: Lens' UpdateMaintenanceWindow (Maybe Bool)
umwAllowUnassociatedTargets = lens _umwAllowUnassociatedTargets (\ s a -> s{_umwAllowUnassociatedTargets = a})

-- | An optional description for the update request.
umwDescription :: Lens' UpdateMaintenanceWindow (Maybe Text)
umwDescription = lens _umwDescription (\ s a -> s{_umwDescription = a}) . mapping _Sensitive

-- | The duration of the Maintenance Window in hours.
umwDuration :: Lens' UpdateMaintenanceWindow (Maybe Natural)
umwDuration = lens _umwDuration (\ s a -> s{_umwDuration = a}) . mapping _Nat

-- | The ID of the Maintenance Window to update.
umwWindowId :: Lens' UpdateMaintenanceWindow Text
umwWindowId = lens _umwWindowId (\ s a -> s{_umwWindowId = a})

instance AWSRequest UpdateMaintenanceWindow where
        type Rs UpdateMaintenanceWindow =
             UpdateMaintenanceWindowResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateMaintenanceWindowResponse' <$>
                   (x .?> "Enabled") <*> (x .?> "Schedule") <*>
                     (x .?> "Name")
                     <*> (x .?> "Cutoff")
                     <*> (x .?> "AllowUnassociatedTargets")
                     <*> (x .?> "Description")
                     <*> (x .?> "Duration")
                     <*> (x .?> "WindowId")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateMaintenanceWindow where

instance NFData UpdateMaintenanceWindow where

instance ToHeaders UpdateMaintenanceWindow where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdateMaintenanceWindow" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateMaintenanceWindow where
        toJSON UpdateMaintenanceWindow'{..}
          = object
              (catMaybes
                 [("Replace" .=) <$> _umwReplace,
                  ("Enabled" .=) <$> _umwEnabled,
                  ("Schedule" .=) <$> _umwSchedule,
                  ("Name" .=) <$> _umwName,
                  ("Cutoff" .=) <$> _umwCutoff,
                  ("AllowUnassociatedTargets" .=) <$>
                    _umwAllowUnassociatedTargets,
                  ("Description" .=) <$> _umwDescription,
                  ("Duration" .=) <$> _umwDuration,
                  Just ("WindowId" .= _umwWindowId)])

instance ToPath UpdateMaintenanceWindow where
        toPath = const "/"

instance ToQuery UpdateMaintenanceWindow where
        toQuery = const mempty

-- | /See:/ 'updateMaintenanceWindowResponse' smart constructor.
data UpdateMaintenanceWindowResponse = UpdateMaintenanceWindowResponse'
  { _umwrsEnabled                  :: !(Maybe Bool)
  , _umwrsSchedule                 :: !(Maybe Text)
  , _umwrsName                     :: !(Maybe Text)
  , _umwrsCutoff                   :: !(Maybe Nat)
  , _umwrsAllowUnassociatedTargets :: !(Maybe Bool)
  , _umwrsDescription              :: !(Maybe (Sensitive Text))
  , _umwrsDuration                 :: !(Maybe Nat)
  , _umwrsWindowId                 :: !(Maybe Text)
  , _umwrsResponseStatus           :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umwrsEnabled' - Whether the Maintenance Window is enabled.
--
-- * 'umwrsSchedule' - The schedule of the Maintenance Window in the form of a cron or rate expression.
--
-- * 'umwrsName' - The name of the Maintenance Window.
--
-- * 'umwrsCutoff' - The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
--
-- * 'umwrsAllowUnassociatedTargets' - Whether targets must be registered with the Maintenance Window before tasks can be defined for those targets.
--
-- * 'umwrsDescription' - An optional description of the update.
--
-- * 'umwrsDuration' - The duration of the Maintenance Window in hours.
--
-- * 'umwrsWindowId' - The ID of the created Maintenance Window.
--
-- * 'umwrsResponseStatus' - -- | The response status code.
updateMaintenanceWindowResponse
    :: Int -- ^ 'umwrsResponseStatus'
    -> UpdateMaintenanceWindowResponse
updateMaintenanceWindowResponse pResponseStatus_ =
  UpdateMaintenanceWindowResponse'
    { _umwrsEnabled = Nothing
    , _umwrsSchedule = Nothing
    , _umwrsName = Nothing
    , _umwrsCutoff = Nothing
    , _umwrsAllowUnassociatedTargets = Nothing
    , _umwrsDescription = Nothing
    , _umwrsDuration = Nothing
    , _umwrsWindowId = Nothing
    , _umwrsResponseStatus = pResponseStatus_
    }


-- | Whether the Maintenance Window is enabled.
umwrsEnabled :: Lens' UpdateMaintenanceWindowResponse (Maybe Bool)
umwrsEnabled = lens _umwrsEnabled (\ s a -> s{_umwrsEnabled = a})

-- | The schedule of the Maintenance Window in the form of a cron or rate expression.
umwrsSchedule :: Lens' UpdateMaintenanceWindowResponse (Maybe Text)
umwrsSchedule = lens _umwrsSchedule (\ s a -> s{_umwrsSchedule = a})

-- | The name of the Maintenance Window.
umwrsName :: Lens' UpdateMaintenanceWindowResponse (Maybe Text)
umwrsName = lens _umwrsName (\ s a -> s{_umwrsName = a})

-- | The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
umwrsCutoff :: Lens' UpdateMaintenanceWindowResponse (Maybe Natural)
umwrsCutoff = lens _umwrsCutoff (\ s a -> s{_umwrsCutoff = a}) . mapping _Nat

-- | Whether targets must be registered with the Maintenance Window before tasks can be defined for those targets.
umwrsAllowUnassociatedTargets :: Lens' UpdateMaintenanceWindowResponse (Maybe Bool)
umwrsAllowUnassociatedTargets = lens _umwrsAllowUnassociatedTargets (\ s a -> s{_umwrsAllowUnassociatedTargets = a})

-- | An optional description of the update.
umwrsDescription :: Lens' UpdateMaintenanceWindowResponse (Maybe Text)
umwrsDescription = lens _umwrsDescription (\ s a -> s{_umwrsDescription = a}) . mapping _Sensitive

-- | The duration of the Maintenance Window in hours.
umwrsDuration :: Lens' UpdateMaintenanceWindowResponse (Maybe Natural)
umwrsDuration = lens _umwrsDuration (\ s a -> s{_umwrsDuration = a}) . mapping _Nat

-- | The ID of the created Maintenance Window.
umwrsWindowId :: Lens' UpdateMaintenanceWindowResponse (Maybe Text)
umwrsWindowId = lens _umwrsWindowId (\ s a -> s{_umwrsWindowId = a})

-- | -- | The response status code.
umwrsResponseStatus :: Lens' UpdateMaintenanceWindowResponse Int
umwrsResponseStatus = lens _umwrsResponseStatus (\ s a -> s{_umwrsResponseStatus = a})

instance NFData UpdateMaintenanceWindowResponse where
