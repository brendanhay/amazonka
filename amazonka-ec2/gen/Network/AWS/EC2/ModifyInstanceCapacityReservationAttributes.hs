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
-- Module      : Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Capacity Reservation settings for a stopped instance. Use this action to configure an instance to target a specific Capacity Reservation, run in any @open@ Capacity Reservation with matching attributes, or run On-Demand Instance capacity.
--
--
module Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes
    (
    -- * Creating a Request
      modifyInstanceCapacityReservationAttributes
    , ModifyInstanceCapacityReservationAttributes
    -- * Request Lenses
    , micraDryRun
    , micraInstanceId
    , micraCapacityReservationSpecification

    -- * Destructuring the Response
    , modifyInstanceCapacityReservationAttributesResponse
    , ModifyInstanceCapacityReservationAttributesResponse
    -- * Response Lenses
    , micrarsReturn
    , micrarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyInstanceCapacityReservationAttributes' smart constructor.
data ModifyInstanceCapacityReservationAttributes = ModifyInstanceCapacityReservationAttributes'
  { _micraDryRun                           :: !(Maybe Bool)
  , _micraInstanceId                       :: !Text
  , _micraCapacityReservationSpecification :: !CapacityReservationSpecification
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceCapacityReservationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'micraDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'micraInstanceId' - The ID of the instance to be modified.
--
-- * 'micraCapacityReservationSpecification' - Information about the Capacity Reservation targeting option.
modifyInstanceCapacityReservationAttributes
    :: Text -- ^ 'micraInstanceId'
    -> CapacityReservationSpecification -- ^ 'micraCapacityReservationSpecification'
    -> ModifyInstanceCapacityReservationAttributes
modifyInstanceCapacityReservationAttributes pInstanceId_ pCapacityReservationSpecification_ =
  ModifyInstanceCapacityReservationAttributes'
    { _micraDryRun = Nothing
    , _micraInstanceId = pInstanceId_
    , _micraCapacityReservationSpecification =
        pCapacityReservationSpecification_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
micraDryRun :: Lens' ModifyInstanceCapacityReservationAttributes (Maybe Bool)
micraDryRun = lens _micraDryRun (\ s a -> s{_micraDryRun = a})

-- | The ID of the instance to be modified.
micraInstanceId :: Lens' ModifyInstanceCapacityReservationAttributes Text
micraInstanceId = lens _micraInstanceId (\ s a -> s{_micraInstanceId = a})

-- | Information about the Capacity Reservation targeting option.
micraCapacityReservationSpecification :: Lens' ModifyInstanceCapacityReservationAttributes CapacityReservationSpecification
micraCapacityReservationSpecification = lens _micraCapacityReservationSpecification (\ s a -> s{_micraCapacityReservationSpecification = a})

instance AWSRequest
           ModifyInstanceCapacityReservationAttributes
         where
        type Rs ModifyInstanceCapacityReservationAttributes =
             ModifyInstanceCapacityReservationAttributesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyInstanceCapacityReservationAttributesResponse'
                   <$> (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable
           ModifyInstanceCapacityReservationAttributes
         where

instance NFData
           ModifyInstanceCapacityReservationAttributes
         where

instance ToHeaders
           ModifyInstanceCapacityReservationAttributes
         where
        toHeaders = const mempty

instance ToPath
           ModifyInstanceCapacityReservationAttributes
         where
        toPath = const "/"

instance ToQuery
           ModifyInstanceCapacityReservationAttributes
         where
        toQuery
          ModifyInstanceCapacityReservationAttributes'{..}
          = mconcat
              ["Action" =:
                 ("ModifyInstanceCapacityReservationAttributes" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _micraDryRun,
               "InstanceId" =: _micraInstanceId,
               "CapacityReservationSpecification" =:
                 _micraCapacityReservationSpecification]

-- | /See:/ 'modifyInstanceCapacityReservationAttributesResponse' smart constructor.
data ModifyInstanceCapacityReservationAttributesResponse = ModifyInstanceCapacityReservationAttributesResponse'
  { _micrarsReturn         :: !(Maybe Bool)
  , _micrarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceCapacityReservationAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'micrarsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'micrarsResponseStatus' - -- | The response status code.
modifyInstanceCapacityReservationAttributesResponse
    :: Int -- ^ 'micrarsResponseStatus'
    -> ModifyInstanceCapacityReservationAttributesResponse
modifyInstanceCapacityReservationAttributesResponse pResponseStatus_ =
  ModifyInstanceCapacityReservationAttributesResponse'
    {_micrarsReturn = Nothing, _micrarsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
micrarsReturn :: Lens' ModifyInstanceCapacityReservationAttributesResponse (Maybe Bool)
micrarsReturn = lens _micrarsReturn (\ s a -> s{_micrarsReturn = a})

-- | -- | The response status code.
micrarsResponseStatus :: Lens' ModifyInstanceCapacityReservationAttributesResponse Int
micrarsResponseStatus = lens _micrarsResponseStatus (\ s a -> s{_micrarsResponseStatus = a})

instance NFData
           ModifyInstanceCapacityReservationAttributesResponse
         where
