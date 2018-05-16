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
-- Module      : Network.AWS.SSM.GetMaintenanceWindow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a Maintenance Window.
--
--
module Network.AWS.SSM.GetMaintenanceWindow
    (
    -- * Creating a Request
      getMaintenanceWindow
    , GetMaintenanceWindow
    -- * Request Lenses
    , gmwWindowId

    -- * Destructuring the Response
    , getMaintenanceWindowResponse
    , GetMaintenanceWindowResponse
    -- * Response Lenses
    , gmwrsEnabled
    , gmwrsSchedule
    , gmwrsCreatedDate
    , gmwrsName
    , gmwrsModifiedDate
    , gmwrsCutoff
    , gmwrsAllowUnassociatedTargets
    , gmwrsDescription
    , gmwrsDuration
    , gmwrsWindowId
    , gmwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getMaintenanceWindow' smart constructor.
newtype GetMaintenanceWindow = GetMaintenanceWindow'
  { _gmwWindowId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwWindowId' - The ID of the desired Maintenance Window.
getMaintenanceWindow
    :: Text -- ^ 'gmwWindowId'
    -> GetMaintenanceWindow
getMaintenanceWindow pWindowId_ =
  GetMaintenanceWindow' {_gmwWindowId = pWindowId_}


-- | The ID of the desired Maintenance Window.
gmwWindowId :: Lens' GetMaintenanceWindow Text
gmwWindowId = lens _gmwWindowId (\ s a -> s{_gmwWindowId = a})

instance AWSRequest GetMaintenanceWindow where
        type Rs GetMaintenanceWindow =
             GetMaintenanceWindowResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetMaintenanceWindowResponse' <$>
                   (x .?> "Enabled") <*> (x .?> "Schedule") <*>
                     (x .?> "CreatedDate")
                     <*> (x .?> "Name")
                     <*> (x .?> "ModifiedDate")
                     <*> (x .?> "Cutoff")
                     <*> (x .?> "AllowUnassociatedTargets")
                     <*> (x .?> "Description")
                     <*> (x .?> "Duration")
                     <*> (x .?> "WindowId")
                     <*> (pure (fromEnum s)))

instance Hashable GetMaintenanceWindow where

instance NFData GetMaintenanceWindow where

instance ToHeaders GetMaintenanceWindow where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetMaintenanceWindow" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMaintenanceWindow where
        toJSON GetMaintenanceWindow'{..}
          = object
              (catMaybes [Just ("WindowId" .= _gmwWindowId)])

instance ToPath GetMaintenanceWindow where
        toPath = const "/"

instance ToQuery GetMaintenanceWindow where
        toQuery = const mempty

-- | /See:/ 'getMaintenanceWindowResponse' smart constructor.
data GetMaintenanceWindowResponse = GetMaintenanceWindowResponse'
  { _gmwrsEnabled                  :: !(Maybe Bool)
  , _gmwrsSchedule                 :: !(Maybe Text)
  , _gmwrsCreatedDate              :: !(Maybe POSIX)
  , _gmwrsName                     :: !(Maybe Text)
  , _gmwrsModifiedDate             :: !(Maybe POSIX)
  , _gmwrsCutoff                   :: !(Maybe Nat)
  , _gmwrsAllowUnassociatedTargets :: !(Maybe Bool)
  , _gmwrsDescription              :: !(Maybe (Sensitive Text))
  , _gmwrsDuration                 :: !(Maybe Nat)
  , _gmwrsWindowId                 :: !(Maybe Text)
  , _gmwrsResponseStatus           :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwrsEnabled' - Whether the Maintenance Windows is enabled.
--
-- * 'gmwrsSchedule' - The schedule of the Maintenance Window in the form of a cron or rate expression.
--
-- * 'gmwrsCreatedDate' - The date the Maintenance Window was created.
--
-- * 'gmwrsName' - The name of the Maintenance Window.
--
-- * 'gmwrsModifiedDate' - The date the Maintenance Window was last modified.
--
-- * 'gmwrsCutoff' - The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
--
-- * 'gmwrsAllowUnassociatedTargets' - Whether targets must be registered with the Maintenance Window before tasks can be defined for those targets.
--
-- * 'gmwrsDescription' - The description of the Maintenance Window.
--
-- * 'gmwrsDuration' - The duration of the Maintenance Window in hours.
--
-- * 'gmwrsWindowId' - The ID of the created Maintenance Window.
--
-- * 'gmwrsResponseStatus' - -- | The response status code.
getMaintenanceWindowResponse
    :: Int -- ^ 'gmwrsResponseStatus'
    -> GetMaintenanceWindowResponse
getMaintenanceWindowResponse pResponseStatus_ =
  GetMaintenanceWindowResponse'
    { _gmwrsEnabled = Nothing
    , _gmwrsSchedule = Nothing
    , _gmwrsCreatedDate = Nothing
    , _gmwrsName = Nothing
    , _gmwrsModifiedDate = Nothing
    , _gmwrsCutoff = Nothing
    , _gmwrsAllowUnassociatedTargets = Nothing
    , _gmwrsDescription = Nothing
    , _gmwrsDuration = Nothing
    , _gmwrsWindowId = Nothing
    , _gmwrsResponseStatus = pResponseStatus_
    }


-- | Whether the Maintenance Windows is enabled.
gmwrsEnabled :: Lens' GetMaintenanceWindowResponse (Maybe Bool)
gmwrsEnabled = lens _gmwrsEnabled (\ s a -> s{_gmwrsEnabled = a})

-- | The schedule of the Maintenance Window in the form of a cron or rate expression.
gmwrsSchedule :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsSchedule = lens _gmwrsSchedule (\ s a -> s{_gmwrsSchedule = a})

-- | The date the Maintenance Window was created.
gmwrsCreatedDate :: Lens' GetMaintenanceWindowResponse (Maybe UTCTime)
gmwrsCreatedDate = lens _gmwrsCreatedDate (\ s a -> s{_gmwrsCreatedDate = a}) . mapping _Time

-- | The name of the Maintenance Window.
gmwrsName :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsName = lens _gmwrsName (\ s a -> s{_gmwrsName = a})

-- | The date the Maintenance Window was last modified.
gmwrsModifiedDate :: Lens' GetMaintenanceWindowResponse (Maybe UTCTime)
gmwrsModifiedDate = lens _gmwrsModifiedDate (\ s a -> s{_gmwrsModifiedDate = a}) . mapping _Time

-- | The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
gmwrsCutoff :: Lens' GetMaintenanceWindowResponse (Maybe Natural)
gmwrsCutoff = lens _gmwrsCutoff (\ s a -> s{_gmwrsCutoff = a}) . mapping _Nat

-- | Whether targets must be registered with the Maintenance Window before tasks can be defined for those targets.
gmwrsAllowUnassociatedTargets :: Lens' GetMaintenanceWindowResponse (Maybe Bool)
gmwrsAllowUnassociatedTargets = lens _gmwrsAllowUnassociatedTargets (\ s a -> s{_gmwrsAllowUnassociatedTargets = a})

-- | The description of the Maintenance Window.
gmwrsDescription :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsDescription = lens _gmwrsDescription (\ s a -> s{_gmwrsDescription = a}) . mapping _Sensitive

-- | The duration of the Maintenance Window in hours.
gmwrsDuration :: Lens' GetMaintenanceWindowResponse (Maybe Natural)
gmwrsDuration = lens _gmwrsDuration (\ s a -> s{_gmwrsDuration = a}) . mapping _Nat

-- | The ID of the created Maintenance Window.
gmwrsWindowId :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsWindowId = lens _gmwrsWindowId (\ s a -> s{_gmwrsWindowId = a})

-- | -- | The response status code.
gmwrsResponseStatus :: Lens' GetMaintenanceWindowResponse Int
gmwrsResponseStatus = lens _gmwrsResponseStatus (\ s a -> s{_gmwrsResponseStatus = a})

instance NFData GetMaintenanceWindowResponse where
