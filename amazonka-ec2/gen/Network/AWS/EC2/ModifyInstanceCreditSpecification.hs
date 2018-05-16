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
-- Module      : Network.AWS.EC2.ModifyInstanceCreditSpecification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the credit option for CPU usage on a running or stopped T2 instance. The credit options are @standard@ and @unlimited@ .
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/t2-instances.html T2 Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.ModifyInstanceCreditSpecification
    (
    -- * Creating a Request
      modifyInstanceCreditSpecification
    , ModifyInstanceCreditSpecification
    -- * Request Lenses
    , micsClientToken
    , micsDryRun
    , micsInstanceCreditSpecifications

    -- * Destructuring the Response
    , modifyInstanceCreditSpecificationResponse
    , ModifyInstanceCreditSpecificationResponse
    -- * Response Lenses
    , micsrsUnsuccessfulInstanceCreditSpecifications
    , micsrsSuccessfulInstanceCreditSpecifications
    , micsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyInstanceCreditSpecification' smart constructor.
data ModifyInstanceCreditSpecification = ModifyInstanceCreditSpecification'
  { _micsClientToken                  :: !(Maybe Text)
  , _micsDryRun                       :: !(Maybe Bool)
  , _micsInstanceCreditSpecifications :: ![InstanceCreditSpecificationRequest]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceCreditSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'micsClientToken' - A unique, case-sensitive token that you provide to ensure idempotency of your modification request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'micsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'micsInstanceCreditSpecifications' - Information about the credit option for CPU usage.
modifyInstanceCreditSpecification
    :: ModifyInstanceCreditSpecification
modifyInstanceCreditSpecification =
  ModifyInstanceCreditSpecification'
    { _micsClientToken = Nothing
    , _micsDryRun = Nothing
    , _micsInstanceCreditSpecifications = mempty
    }


-- | A unique, case-sensitive token that you provide to ensure idempotency of your modification request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
micsClientToken :: Lens' ModifyInstanceCreditSpecification (Maybe Text)
micsClientToken = lens _micsClientToken (\ s a -> s{_micsClientToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
micsDryRun :: Lens' ModifyInstanceCreditSpecification (Maybe Bool)
micsDryRun = lens _micsDryRun (\ s a -> s{_micsDryRun = a})

-- | Information about the credit option for CPU usage.
micsInstanceCreditSpecifications :: Lens' ModifyInstanceCreditSpecification [InstanceCreditSpecificationRequest]
micsInstanceCreditSpecifications = lens _micsInstanceCreditSpecifications (\ s a -> s{_micsInstanceCreditSpecifications = a}) . _Coerce

instance AWSRequest ModifyInstanceCreditSpecification
         where
        type Rs ModifyInstanceCreditSpecification =
             ModifyInstanceCreditSpecificationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyInstanceCreditSpecificationResponse' <$>
                   (x .@? "unsuccessfulInstanceCreditSpecificationSet"
                      .!@ mempty
                      >>= may (parseXMLList "item"))
                     <*>
                     (x .@? "successfulInstanceCreditSpecificationSet" .!@
                        mempty
                        >>= may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable ModifyInstanceCreditSpecification
         where

instance NFData ModifyInstanceCreditSpecification
         where

instance ToHeaders ModifyInstanceCreditSpecification
         where
        toHeaders = const mempty

instance ToPath ModifyInstanceCreditSpecification
         where
        toPath = const "/"

instance ToQuery ModifyInstanceCreditSpecification
         where
        toQuery ModifyInstanceCreditSpecification'{..}
          = mconcat
              ["Action" =:
                 ("ModifyInstanceCreditSpecification" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _micsClientToken,
               "DryRun" =: _micsDryRun,
               toQueryList "InstanceCreditSpecification"
                 _micsInstanceCreditSpecifications]

-- | /See:/ 'modifyInstanceCreditSpecificationResponse' smart constructor.
data ModifyInstanceCreditSpecificationResponse = ModifyInstanceCreditSpecificationResponse'
  { _micsrsUnsuccessfulInstanceCreditSpecifications :: !(Maybe [UnsuccessfulInstanceCreditSpecificationItem])
  , _micsrsSuccessfulInstanceCreditSpecifications :: !(Maybe [SuccessfulInstanceCreditSpecificationItem])
  , _micsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceCreditSpecificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'micsrsUnsuccessfulInstanceCreditSpecifications' - Information about the instances whose credit option for CPU usage was not modified.
--
-- * 'micsrsSuccessfulInstanceCreditSpecifications' - Information about the instances whose credit option for CPU usage was successfully modified.
--
-- * 'micsrsResponseStatus' - -- | The response status code.
modifyInstanceCreditSpecificationResponse
    :: Int -- ^ 'micsrsResponseStatus'
    -> ModifyInstanceCreditSpecificationResponse
modifyInstanceCreditSpecificationResponse pResponseStatus_ =
  ModifyInstanceCreditSpecificationResponse'
    { _micsrsUnsuccessfulInstanceCreditSpecifications = Nothing
    , _micsrsSuccessfulInstanceCreditSpecifications = Nothing
    , _micsrsResponseStatus = pResponseStatus_
    }


-- | Information about the instances whose credit option for CPU usage was not modified.
micsrsUnsuccessfulInstanceCreditSpecifications :: Lens' ModifyInstanceCreditSpecificationResponse [UnsuccessfulInstanceCreditSpecificationItem]
micsrsUnsuccessfulInstanceCreditSpecifications = lens _micsrsUnsuccessfulInstanceCreditSpecifications (\ s a -> s{_micsrsUnsuccessfulInstanceCreditSpecifications = a}) . _Default . _Coerce

-- | Information about the instances whose credit option for CPU usage was successfully modified.
micsrsSuccessfulInstanceCreditSpecifications :: Lens' ModifyInstanceCreditSpecificationResponse [SuccessfulInstanceCreditSpecificationItem]
micsrsSuccessfulInstanceCreditSpecifications = lens _micsrsSuccessfulInstanceCreditSpecifications (\ s a -> s{_micsrsSuccessfulInstanceCreditSpecifications = a}) . _Default . _Coerce

-- | -- | The response status code.
micsrsResponseStatus :: Lens' ModifyInstanceCreditSpecificationResponse Int
micsrsResponseStatus = lens _micsrsResponseStatus (\ s a -> s{_micsrsResponseStatus = a})

instance NFData
           ModifyInstanceCreditSpecificationResponse
         where
