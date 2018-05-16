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
-- Module      : Network.AWS.CodeDeploy.RegisterOnPremisesInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an on-premises instance.
--
--
module Network.AWS.CodeDeploy.RegisterOnPremisesInstance
    (
    -- * Creating a Request
      registerOnPremisesInstance
    , RegisterOnPremisesInstance
    -- * Request Lenses
    , ropiIamUserARN
    , ropiIamSessionARN
    , ropiInstanceName

    -- * Destructuring the Response
    , registerOnPremisesInstanceResponse
    , RegisterOnPremisesInstanceResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of the register on-premises instance operation.
--
--
--
-- /See:/ 'registerOnPremisesInstance' smart constructor.
data RegisterOnPremisesInstance = RegisterOnPremisesInstance'
  { _ropiIamUserARN    :: !(Maybe Text)
  , _ropiIamSessionARN :: !(Maybe Text)
  , _ropiInstanceName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterOnPremisesInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ropiIamUserARN' - The ARN of the IAM user to associate with the on-premises instance.
--
-- * 'ropiIamSessionARN' - The ARN of the IAM session to associate with the on-premises instance.
--
-- * 'ropiInstanceName' - The name of the on-premises instance to register.
registerOnPremisesInstance
    :: Text -- ^ 'ropiInstanceName'
    -> RegisterOnPremisesInstance
registerOnPremisesInstance pInstanceName_ =
  RegisterOnPremisesInstance'
    { _ropiIamUserARN = Nothing
    , _ropiIamSessionARN = Nothing
    , _ropiInstanceName = pInstanceName_
    }


-- | The ARN of the IAM user to associate with the on-premises instance.
ropiIamUserARN :: Lens' RegisterOnPremisesInstance (Maybe Text)
ropiIamUserARN = lens _ropiIamUserARN (\ s a -> s{_ropiIamUserARN = a})

-- | The ARN of the IAM session to associate with the on-premises instance.
ropiIamSessionARN :: Lens' RegisterOnPremisesInstance (Maybe Text)
ropiIamSessionARN = lens _ropiIamSessionARN (\ s a -> s{_ropiIamSessionARN = a})

-- | The name of the on-premises instance to register.
ropiInstanceName :: Lens' RegisterOnPremisesInstance Text
ropiInstanceName = lens _ropiInstanceName (\ s a -> s{_ropiInstanceName = a})

instance AWSRequest RegisterOnPremisesInstance where
        type Rs RegisterOnPremisesInstance =
             RegisterOnPremisesInstanceResponse
        request = postJSON codeDeploy
        response
          = receiveNull RegisterOnPremisesInstanceResponse'

instance Hashable RegisterOnPremisesInstance where

instance NFData RegisterOnPremisesInstance where

instance ToHeaders RegisterOnPremisesInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.RegisterOnPremisesInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterOnPremisesInstance where
        toJSON RegisterOnPremisesInstance'{..}
          = object
              (catMaybes
                 [("iamUserArn" .=) <$> _ropiIamUserARN,
                  ("iamSessionArn" .=) <$> _ropiIamSessionARN,
                  Just ("instanceName" .= _ropiInstanceName)])

instance ToPath RegisterOnPremisesInstance where
        toPath = const "/"

instance ToQuery RegisterOnPremisesInstance where
        toQuery = const mempty

-- | /See:/ 'registerOnPremisesInstanceResponse' smart constructor.
data RegisterOnPremisesInstanceResponse =
  RegisterOnPremisesInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterOnPremisesInstanceResponse' with the minimum fields required to make a request.
--
registerOnPremisesInstanceResponse
    :: RegisterOnPremisesInstanceResponse
registerOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse'


instance NFData RegisterOnPremisesInstanceResponse
         where
