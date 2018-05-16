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
-- Module      : Network.AWS.SSM.CreateActivation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers your on-premises server or virtual machine with Amazon EC2 so that you can manage these resources using Run Command. An on-premises server or virtual machine that has been registered with EC2 is called a managed instance. For more information about activations, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances.html Setting Up Systems Manager in Hybrid Environments> .
--
--
module Network.AWS.SSM.CreateActivation
    (
    -- * Creating a Request
      createActivation
    , CreateActivation
    -- * Request Lenses
    , caDefaultInstanceName
    , caRegistrationLimit
    , caExpirationDate
    , caDescription
    , caIAMRole

    -- * Destructuring the Response
    , createActivationResponse
    , CreateActivationResponse
    -- * Response Lenses
    , carsActivationId
    , carsActivationCode
    , carsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'createActivation' smart constructor.
data CreateActivation = CreateActivation'
  { _caDefaultInstanceName :: !(Maybe Text)
  , _caRegistrationLimit   :: !(Maybe Nat)
  , _caExpirationDate      :: !(Maybe POSIX)
  , _caDescription         :: !(Maybe Text)
  , _caIAMRole             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateActivation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caDefaultInstanceName' - The name of the registered, managed instance as it will appear in the Amazon EC2 console or when you use the AWS command line tools to list EC2 resources. /Important:/ Do not enter personally identifiable information in this field.
--
-- * 'caRegistrationLimit' - Specify the maximum number of managed instances you want to register. The default value is 1 instance.
--
-- * 'caExpirationDate' - The date by which this activation request should expire. The default value is 24 hours.
--
-- * 'caDescription' - A user-defined description of the resource that you want to register with Amazon EC2.  /Important:/ Do not enter personally identifiable information in this field.
--
-- * 'caIAMRole' - The Amazon Identity and Access Management (IAM) role that you want to assign to the managed instance.
createActivation
    :: Text -- ^ 'caIAMRole'
    -> CreateActivation
createActivation pIAMRole_ =
  CreateActivation'
    { _caDefaultInstanceName = Nothing
    , _caRegistrationLimit = Nothing
    , _caExpirationDate = Nothing
    , _caDescription = Nothing
    , _caIAMRole = pIAMRole_
    }


-- | The name of the registered, managed instance as it will appear in the Amazon EC2 console or when you use the AWS command line tools to list EC2 resources. /Important:/ Do not enter personally identifiable information in this field.
caDefaultInstanceName :: Lens' CreateActivation (Maybe Text)
caDefaultInstanceName = lens _caDefaultInstanceName (\ s a -> s{_caDefaultInstanceName = a})

-- | Specify the maximum number of managed instances you want to register. The default value is 1 instance.
caRegistrationLimit :: Lens' CreateActivation (Maybe Natural)
caRegistrationLimit = lens _caRegistrationLimit (\ s a -> s{_caRegistrationLimit = a}) . mapping _Nat

-- | The date by which this activation request should expire. The default value is 24 hours.
caExpirationDate :: Lens' CreateActivation (Maybe UTCTime)
caExpirationDate = lens _caExpirationDate (\ s a -> s{_caExpirationDate = a}) . mapping _Time

-- | A user-defined description of the resource that you want to register with Amazon EC2.  /Important:/ Do not enter personally identifiable information in this field.
caDescription :: Lens' CreateActivation (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | The Amazon Identity and Access Management (IAM) role that you want to assign to the managed instance.
caIAMRole :: Lens' CreateActivation Text
caIAMRole = lens _caIAMRole (\ s a -> s{_caIAMRole = a})

instance AWSRequest CreateActivation where
        type Rs CreateActivation = CreateActivationResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreateActivationResponse' <$>
                   (x .?> "ActivationId") <*> (x .?> "ActivationCode")
                     <*> (pure (fromEnum s)))

instance Hashable CreateActivation where

instance NFData CreateActivation where

instance ToHeaders CreateActivation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreateActivation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateActivation where
        toJSON CreateActivation'{..}
          = object
              (catMaybes
                 [("DefaultInstanceName" .=) <$>
                    _caDefaultInstanceName,
                  ("RegistrationLimit" .=) <$> _caRegistrationLimit,
                  ("ExpirationDate" .=) <$> _caExpirationDate,
                  ("Description" .=) <$> _caDescription,
                  Just ("IamRole" .= _caIAMRole)])

instance ToPath CreateActivation where
        toPath = const "/"

instance ToQuery CreateActivation where
        toQuery = const mempty

-- | /See:/ 'createActivationResponse' smart constructor.
data CreateActivationResponse = CreateActivationResponse'
  { _carsActivationId   :: !(Maybe Text)
  , _carsActivationCode :: !(Maybe Text)
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateActivationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsActivationId' - The ID number generated by the system when it processed the activation. The activation ID functions like a user name.
--
-- * 'carsActivationCode' - The code the system generates when it processes the activation. The activation code functions like a password to validate the activation ID.
--
-- * 'carsResponseStatus' - -- | The response status code.
createActivationResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateActivationResponse
createActivationResponse pResponseStatus_ =
  CreateActivationResponse'
    { _carsActivationId = Nothing
    , _carsActivationCode = Nothing
    , _carsResponseStatus = pResponseStatus_
    }


-- | The ID number generated by the system when it processed the activation. The activation ID functions like a user name.
carsActivationId :: Lens' CreateActivationResponse (Maybe Text)
carsActivationId = lens _carsActivationId (\ s a -> s{_carsActivationId = a})

-- | The code the system generates when it processes the activation. The activation code functions like a password to validate the activation ID.
carsActivationCode :: Lens' CreateActivationResponse (Maybe Text)
carsActivationCode = lens _carsActivationCode (\ s a -> s{_carsActivationCode = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateActivationResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateActivationResponse where
