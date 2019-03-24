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
-- Module      : Network.AWS.EC2.ModifyCapacityReservation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Capacity Reservation's capacity and the conditions under which it is to be released. You cannot change a Capacity Reservation's instance type, EBS optimization, instance store settings, platform, Availability Zone, or instance eligibility. If you need to modify any of these attributes, we recommend that you cancel the Capacity Reservation, and then create a new one with the required attributes.
--
--
module Network.AWS.EC2.ModifyCapacityReservation
    (
    -- * Creating a Request
      modifyCapacityReservation
    , ModifyCapacityReservation
    -- * Request Lenses
    , mcrInstanceCount
    , mcrEndDate
    , mcrEndDateType
    , mcrDryRun
    , mcrCapacityReservationId

    -- * Destructuring the Response
    , modifyCapacityReservationResponse
    , ModifyCapacityReservationResponse
    -- * Response Lenses
    , mcrrsReturn
    , mcrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyCapacityReservation' smart constructor.
data ModifyCapacityReservation = ModifyCapacityReservation'
  { _mcrInstanceCount         :: !(Maybe Int)
  , _mcrEndDate               :: !(Maybe ISO8601)
  , _mcrEndDateType           :: !(Maybe EndDateType)
  , _mcrDryRun                :: !(Maybe Bool)
  , _mcrCapacityReservationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyCapacityReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcrInstanceCount' - The number of instances for which to reserve capacity.
--
-- * 'mcrEndDate' - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time. The Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019. You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
--
-- * 'mcrEndDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ value if @EndDateType@ is @unlimited@ .     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if @EndDateType@ is @limited@ .
--
-- * 'mcrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mcrCapacityReservationId' - The ID of the Capacity Reservation.
modifyCapacityReservation
    :: Text -- ^ 'mcrCapacityReservationId'
    -> ModifyCapacityReservation
modifyCapacityReservation pCapacityReservationId_ =
  ModifyCapacityReservation'
    { _mcrInstanceCount = Nothing
    , _mcrEndDate = Nothing
    , _mcrEndDateType = Nothing
    , _mcrDryRun = Nothing
    , _mcrCapacityReservationId = pCapacityReservationId_
    }


-- | The number of instances for which to reserve capacity.
mcrInstanceCount :: Lens' ModifyCapacityReservation (Maybe Int)
mcrInstanceCount = lens _mcrInstanceCount (\ s a -> s{_mcrInstanceCount = a})

-- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time. The Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019. You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
mcrEndDate :: Lens' ModifyCapacityReservation (Maybe UTCTime)
mcrEndDate = lens _mcrEndDate (\ s a -> s{_mcrEndDate = a}) . mapping _Time

-- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ value if @EndDateType@ is @unlimited@ .     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if @EndDateType@ is @limited@ .
mcrEndDateType :: Lens' ModifyCapacityReservation (Maybe EndDateType)
mcrEndDateType = lens _mcrEndDateType (\ s a -> s{_mcrEndDateType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mcrDryRun :: Lens' ModifyCapacityReservation (Maybe Bool)
mcrDryRun = lens _mcrDryRun (\ s a -> s{_mcrDryRun = a})

-- | The ID of the Capacity Reservation.
mcrCapacityReservationId :: Lens' ModifyCapacityReservation Text
mcrCapacityReservationId = lens _mcrCapacityReservationId (\ s a -> s{_mcrCapacityReservationId = a})

instance AWSRequest ModifyCapacityReservation where
        type Rs ModifyCapacityReservation =
             ModifyCapacityReservationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyCapacityReservationResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ModifyCapacityReservation where

instance NFData ModifyCapacityReservation where

instance ToHeaders ModifyCapacityReservation where
        toHeaders = const mempty

instance ToPath ModifyCapacityReservation where
        toPath = const "/"

instance ToQuery ModifyCapacityReservation where
        toQuery ModifyCapacityReservation'{..}
          = mconcat
              ["Action" =:
                 ("ModifyCapacityReservation" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "InstanceCount" =: _mcrInstanceCount,
               "EndDate" =: _mcrEndDate,
               "EndDateType" =: _mcrEndDateType,
               "DryRun" =: _mcrDryRun,
               "CapacityReservationId" =: _mcrCapacityReservationId]

-- | /See:/ 'modifyCapacityReservationResponse' smart constructor.
data ModifyCapacityReservationResponse = ModifyCapacityReservationResponse'
  { _mcrrsReturn         :: !(Maybe Bool)
  , _mcrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyCapacityReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcrrsReturn' - Information about the Capacity Reservation.
--
-- * 'mcrrsResponseStatus' - -- | The response status code.
modifyCapacityReservationResponse
    :: Int -- ^ 'mcrrsResponseStatus'
    -> ModifyCapacityReservationResponse
modifyCapacityReservationResponse pResponseStatus_ =
  ModifyCapacityReservationResponse'
    {_mcrrsReturn = Nothing, _mcrrsResponseStatus = pResponseStatus_}


-- | Information about the Capacity Reservation.
mcrrsReturn :: Lens' ModifyCapacityReservationResponse (Maybe Bool)
mcrrsReturn = lens _mcrrsReturn (\ s a -> s{_mcrrsReturn = a})

-- | -- | The response status code.
mcrrsResponseStatus :: Lens' ModifyCapacityReservationResponse Int
mcrrsResponseStatus = lens _mcrrsResponseStatus (\ s a -> s{_mcrrsResponseStatus = a})

instance NFData ModifyCapacityReservationResponse
         where
