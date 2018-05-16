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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an on-premises instance.
--
--
module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
    (
    -- * Creating a Request
      deregisterOnPremisesInstance
    , DeregisterOnPremisesInstance
    -- * Request Lenses
    , dopiInstanceName

    -- * Destructuring the Response
    , deregisterOnPremisesInstanceResponse
    , DeregisterOnPremisesInstanceResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a DeregisterOnPremisesInstance operation.
--
--
--
-- /See:/ 'deregisterOnPremisesInstance' smart constructor.
newtype DeregisterOnPremisesInstance = DeregisterOnPremisesInstance'
  { _dopiInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterOnPremisesInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dopiInstanceName' - The name of the on-premises instance to deregister.
deregisterOnPremisesInstance
    :: Text -- ^ 'dopiInstanceName'
    -> DeregisterOnPremisesInstance
deregisterOnPremisesInstance pInstanceName_ =
  DeregisterOnPremisesInstance' {_dopiInstanceName = pInstanceName_}


-- | The name of the on-premises instance to deregister.
dopiInstanceName :: Lens' DeregisterOnPremisesInstance Text
dopiInstanceName = lens _dopiInstanceName (\ s a -> s{_dopiInstanceName = a})

instance AWSRequest DeregisterOnPremisesInstance
         where
        type Rs DeregisterOnPremisesInstance =
             DeregisterOnPremisesInstanceResponse
        request = postJSON codeDeploy
        response
          = receiveNull DeregisterOnPremisesInstanceResponse'

instance Hashable DeregisterOnPremisesInstance where

instance NFData DeregisterOnPremisesInstance where

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
          = object
              (catMaybes
                 [Just ("instanceName" .= _dopiInstanceName)])

instance ToPath DeregisterOnPremisesInstance where
        toPath = const "/"

instance ToQuery DeregisterOnPremisesInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterOnPremisesInstanceResponse' smart constructor.
data DeregisterOnPremisesInstanceResponse =
  DeregisterOnPremisesInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterOnPremisesInstanceResponse' with the minimum fields required to make a request.
--
deregisterOnPremisesInstanceResponse
    :: DeregisterOnPremisesInstanceResponse
deregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse'


instance NFData DeregisterOnPremisesInstanceResponse
         where
