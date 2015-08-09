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
-- Module      : Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an on-premises instance.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_DeregisterOnPremisesInstance.html AWS API Reference> for DeregisterOnPremisesInstance.
module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
    (
    -- * Creating a Request
      DeregisterOnPremisesInstance
    , deregisterOnPremisesInstance
    -- * Request Lenses
    , dopiInstanceName

    -- * Destructuring the Response
    , DeregisterOnPremisesInstanceResponse
    , deregisterOnPremisesInstanceResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a deregister on-premises instance operation.
--
-- /See:/ 'deregisterOnPremisesInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dopiInstanceName'
newtype DeregisterOnPremisesInstance = DeregisterOnPremisesInstance'
    { _dopiInstanceName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterOnPremisesInstance' smart constructor.
deregisterOnPremisesInstance :: Text -> DeregisterOnPremisesInstance
deregisterOnPremisesInstance pInstanceName_ = 
    DeregisterOnPremisesInstance'
    { _dopiInstanceName = pInstanceName_
    }

-- | The name of the on-premises instance to deregister.
dopiInstanceName :: Lens' DeregisterOnPremisesInstance Text
dopiInstanceName = lens _dopiInstanceName (\ s a -> s{_dopiInstanceName = a});

instance AWSRequest DeregisterOnPremisesInstance
         where
        type Sv DeregisterOnPremisesInstance = CodeDeploy
        type Rs DeregisterOnPremisesInstance =
             DeregisterOnPremisesInstanceResponse
        request = postJSON
        response
          = receiveNull DeregisterOnPremisesInstanceResponse'

instance ToHeaders DeregisterOnPremisesInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.DeregisterOnPremisesInstance"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterOnPremisesInstance where
        toJSON DeregisterOnPremisesInstance'{..}
          = object ["instanceName" .= _dopiInstanceName]

instance ToPath DeregisterOnPremisesInstance where
        toPath = const "/"

instance ToQuery DeregisterOnPremisesInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterOnPremisesInstanceResponse' smart constructor.
data DeregisterOnPremisesInstanceResponse =
    DeregisterOnPremisesInstanceResponse' 
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterOnPremisesInstanceResponse' smart constructor.
deregisterOnPremisesInstanceResponse :: DeregisterOnPremisesInstanceResponse
deregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse'
