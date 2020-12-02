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
-- Module      : Network.AWS.Snowball.UpdateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- While a job's @JobState@ value is @New@ , you can update some of the information associated with a job. Once the job changes to a different job state, usually within 60 minutes of the job being created, this action is no longer available.
--
--
module Network.AWS.Snowball.UpdateJob
    (
    -- * Creating a Request
      updateJob
    , UpdateJob
    -- * Request Lenses
    , ujNotification
    , ujForwardingAddressId
    , ujAddressId
    , ujShippingOption
    , ujResources
    , ujDescription
    , ujRoleARN
    , ujSnowballCapacityPreference
    , ujJobId

    -- * Destructuring the Response
    , updateJobResponse
    , UpdateJobResponse
    -- * Response Lenses
    , ujrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'updateJob' smart constructor.
data UpdateJob = UpdateJob'
  { _ujNotification               :: !(Maybe Notification)
  , _ujForwardingAddressId        :: !(Maybe Text)
  , _ujAddressId                  :: !(Maybe Text)
  , _ujShippingOption             :: !(Maybe ShippingOption)
  , _ujResources                  :: !(Maybe JobResource)
  , _ujDescription                :: !(Maybe Text)
  , _ujRoleARN                    :: !(Maybe Text)
  , _ujSnowballCapacityPreference :: !(Maybe SnowballCapacity)
  , _ujJobId                      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujNotification' - The new or updated 'Notification' object.
--
-- * 'ujForwardingAddressId' - The updated ID for the forwarding address for a job. This field is not supported in most regions.
--
-- * 'ujAddressId' - The ID of the updated 'Address' object.
--
-- * 'ujShippingOption' - The updated shipping option value of this job's 'ShippingDetails' object.
--
-- * 'ujResources' - The updated 'S3Resource' object (for a single Amazon S3 bucket or key range), or the updated 'JobResource' object (for multiple buckets or key ranges).
--
-- * 'ujDescription' - The updated description of this job's 'JobMetadata' object.
--
-- * 'ujRoleARN' - The new role Amazon Resource Name (ARN) that you want to associate with this job. To create a role ARN, use the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
--
-- * 'ujSnowballCapacityPreference' - The updated @SnowballCapacityPreference@ of this job's 'JobMetadata' object. The 50 TB Snowballs are only available in the US regions.
--
-- * 'ujJobId' - The job ID of the job that you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
updateJob
    :: Text -- ^ 'ujJobId'
    -> UpdateJob
updateJob pJobId_ =
  UpdateJob'
    { _ujNotification = Nothing
    , _ujForwardingAddressId = Nothing
    , _ujAddressId = Nothing
    , _ujShippingOption = Nothing
    , _ujResources = Nothing
    , _ujDescription = Nothing
    , _ujRoleARN = Nothing
    , _ujSnowballCapacityPreference = Nothing
    , _ujJobId = pJobId_
    }


-- | The new or updated 'Notification' object.
ujNotification :: Lens' UpdateJob (Maybe Notification)
ujNotification = lens _ujNotification (\ s a -> s{_ujNotification = a})

-- | The updated ID for the forwarding address for a job. This field is not supported in most regions.
ujForwardingAddressId :: Lens' UpdateJob (Maybe Text)
ujForwardingAddressId = lens _ujForwardingAddressId (\ s a -> s{_ujForwardingAddressId = a})

-- | The ID of the updated 'Address' object.
ujAddressId :: Lens' UpdateJob (Maybe Text)
ujAddressId = lens _ujAddressId (\ s a -> s{_ujAddressId = a})

-- | The updated shipping option value of this job's 'ShippingDetails' object.
ujShippingOption :: Lens' UpdateJob (Maybe ShippingOption)
ujShippingOption = lens _ujShippingOption (\ s a -> s{_ujShippingOption = a})

-- | The updated 'S3Resource' object (for a single Amazon S3 bucket or key range), or the updated 'JobResource' object (for multiple buckets or key ranges).
ujResources :: Lens' UpdateJob (Maybe JobResource)
ujResources = lens _ujResources (\ s a -> s{_ujResources = a})

-- | The updated description of this job's 'JobMetadata' object.
ujDescription :: Lens' UpdateJob (Maybe Text)
ujDescription = lens _ujDescription (\ s a -> s{_ujDescription = a})

-- | The new role Amazon Resource Name (ARN) that you want to associate with this job. To create a role ARN, use the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
ujRoleARN :: Lens' UpdateJob (Maybe Text)
ujRoleARN = lens _ujRoleARN (\ s a -> s{_ujRoleARN = a})

-- | The updated @SnowballCapacityPreference@ of this job's 'JobMetadata' object. The 50 TB Snowballs are only available in the US regions.
ujSnowballCapacityPreference :: Lens' UpdateJob (Maybe SnowballCapacity)
ujSnowballCapacityPreference = lens _ujSnowballCapacityPreference (\ s a -> s{_ujSnowballCapacityPreference = a})

-- | The job ID of the job that you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
ujJobId :: Lens' UpdateJob Text
ujJobId = lens _ujJobId (\ s a -> s{_ujJobId = a})

instance AWSRequest UpdateJob where
        type Rs UpdateJob = UpdateJobResponse
        request = postJSON snowball
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateJobResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateJob where

instance NFData UpdateJob where

instance ToHeaders UpdateJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.UpdateJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateJob where
        toJSON UpdateJob'{..}
          = object
              (catMaybes
                 [("Notification" .=) <$> _ujNotification,
                  ("ForwardingAddressId" .=) <$>
                    _ujForwardingAddressId,
                  ("AddressId" .=) <$> _ujAddressId,
                  ("ShippingOption" .=) <$> _ujShippingOption,
                  ("Resources" .=) <$> _ujResources,
                  ("Description" .=) <$> _ujDescription,
                  ("RoleARN" .=) <$> _ujRoleARN,
                  ("SnowballCapacityPreference" .=) <$>
                    _ujSnowballCapacityPreference,
                  Just ("JobId" .= _ujJobId)])

instance ToPath UpdateJob where
        toPath = const "/"

instance ToQuery UpdateJob where
        toQuery = const mempty

-- | /See:/ 'updateJobResponse' smart constructor.
newtype UpdateJobResponse = UpdateJobResponse'
  { _ujrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujrsResponseStatus' - -- | The response status code.
updateJobResponse
    :: Int -- ^ 'ujrsResponseStatus'
    -> UpdateJobResponse
updateJobResponse pResponseStatus_ =
  UpdateJobResponse' {_ujrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ujrsResponseStatus :: Lens' UpdateJobResponse Int
ujrsResponseStatus = lens _ujrsResponseStatus (\ s a -> s{_ujrsResponseStatus = a})

instance NFData UpdateJobResponse where
