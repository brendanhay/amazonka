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
-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an update on the specified stack. If the call completes successfully, the stack rolls back the update and reverts to the previous stack configuration.
--
--
module Network.AWS.CloudFormation.CancelUpdateStack
    (
    -- * Creating a Request
      cancelUpdateStack
    , CancelUpdateStack
    -- * Request Lenses
    , cusClientRequestToken
    , cusStackName

    -- * Destructuring the Response
    , cancelUpdateStackResponse
    , CancelUpdateStackResponse
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'CancelUpdateStack' action.
--
--
--
-- /See:/ 'cancelUpdateStack' smart constructor.
data CancelUpdateStack = CancelUpdateStack'
  { _cusClientRequestToken :: !(Maybe Text)
  , _cusStackName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelUpdateStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cusClientRequestToken' - A unique identifier for this @CancelUpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to cancel an update on a stack with the same name. You might retry @CancelUpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- * 'cusStackName' - The name or the unique stack ID that is associated with the stack.
cancelUpdateStack
    :: Text -- ^ 'cusStackName'
    -> CancelUpdateStack
cancelUpdateStack pStackName_ =
  CancelUpdateStack'
    {_cusClientRequestToken = Nothing, _cusStackName = pStackName_}


-- | A unique identifier for this @CancelUpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to cancel an update on a stack with the same name. You might retry @CancelUpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
cusClientRequestToken :: Lens' CancelUpdateStack (Maybe Text)
cusClientRequestToken = lens _cusClientRequestToken (\ s a -> s{_cusClientRequestToken = a})

-- | The name or the unique stack ID that is associated with the stack.
cusStackName :: Lens' CancelUpdateStack Text
cusStackName = lens _cusStackName (\ s a -> s{_cusStackName = a})

instance AWSRequest CancelUpdateStack where
        type Rs CancelUpdateStack = CancelUpdateStackResponse
        request = postQuery cloudFormation
        response = receiveNull CancelUpdateStackResponse'

instance Hashable CancelUpdateStack where

instance NFData CancelUpdateStack where

instance ToHeaders CancelUpdateStack where
        toHeaders = const mempty

instance ToPath CancelUpdateStack where
        toPath = const "/"

instance ToQuery CancelUpdateStack where
        toQuery CancelUpdateStack'{..}
          = mconcat
              ["Action" =: ("CancelUpdateStack" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "ClientRequestToken" =: _cusClientRequestToken,
               "StackName" =: _cusStackName]

-- | /See:/ 'cancelUpdateStackResponse' smart constructor.
data CancelUpdateStackResponse =
  CancelUpdateStackResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelUpdateStackResponse' with the minimum fields required to make a request.
--
cancelUpdateStackResponse
    :: CancelUpdateStackResponse
cancelUpdateStackResponse = CancelUpdateStackResponse'


instance NFData CancelUpdateStackResponse where
