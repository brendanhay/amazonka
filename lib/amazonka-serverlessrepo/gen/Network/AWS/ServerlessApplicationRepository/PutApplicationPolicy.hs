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
-- Module      : Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the policy for the specified application.
--
--
module Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
    (
    -- * Creating a Request
      putApplicationPolicy
    , PutApplicationPolicy
    -- * Request Lenses
    , papStatements
    , papApplicationId

    -- * Destructuring the Response
    , putApplicationPolicyResponse
    , PutApplicationPolicyResponse
    -- * Response Lenses
    , paprsStatements
    , paprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'putApplicationPolicy' smart constructor.
data PutApplicationPolicy = PutApplicationPolicy'
  { _papStatements    :: !(Maybe [ApplicationPolicyStatement])
  , _papApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutApplicationPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'papStatements' - Array of policy statements applied to the application.
--
-- * 'papApplicationId' - The ID of the application to get.
putApplicationPolicy
    :: Text -- ^ 'papApplicationId'
    -> PutApplicationPolicy
putApplicationPolicy pApplicationId_ =
  PutApplicationPolicy'
    {_papStatements = Nothing, _papApplicationId = pApplicationId_}


-- | Array of policy statements applied to the application.
papStatements :: Lens' PutApplicationPolicy [ApplicationPolicyStatement]
papStatements = lens _papStatements (\ s a -> s{_papStatements = a}) . _Default . _Coerce

-- | The ID of the application to get.
papApplicationId :: Lens' PutApplicationPolicy Text
papApplicationId = lens _papApplicationId (\ s a -> s{_papApplicationId = a})

instance AWSRequest PutApplicationPolicy where
        type Rs PutApplicationPolicy =
             PutApplicationPolicyResponse
        request = putJSON serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 PutApplicationPolicyResponse' <$>
                   (x .?> "statements" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable PutApplicationPolicy where

instance NFData PutApplicationPolicy where

instance ToHeaders PutApplicationPolicy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutApplicationPolicy where
        toJSON PutApplicationPolicy'{..}
          = object
              (catMaybes [("statements" .=) <$> _papStatements])

instance ToPath PutApplicationPolicy where
        toPath PutApplicationPolicy'{..}
          = mconcat
              ["/applications/", toBS _papApplicationId, "/policy"]

instance ToQuery PutApplicationPolicy where
        toQuery = const mempty

-- | /See:/ 'putApplicationPolicyResponse' smart constructor.
data PutApplicationPolicyResponse = PutApplicationPolicyResponse'
  { _paprsStatements     :: !(Maybe [ApplicationPolicyStatement])
  , _paprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutApplicationPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paprsStatements' - Array of policy statements applied to the application.
--
-- * 'paprsResponseStatus' - -- | The response status code.
putApplicationPolicyResponse
    :: Int -- ^ 'paprsResponseStatus'
    -> PutApplicationPolicyResponse
putApplicationPolicyResponse pResponseStatus_ =
  PutApplicationPolicyResponse'
    {_paprsStatements = Nothing, _paprsResponseStatus = pResponseStatus_}


-- | Array of policy statements applied to the application.
paprsStatements :: Lens' PutApplicationPolicyResponse [ApplicationPolicyStatement]
paprsStatements = lens _paprsStatements (\ s a -> s{_paprsStatements = a}) . _Default . _Coerce

-- | -- | The response status code.
paprsResponseStatus :: Lens' PutApplicationPolicyResponse Int
paprsResponseStatus = lens _paprsResponseStatus (\ s a -> s{_paprsResponseStatus = a})

instance NFData PutApplicationPolicyResponse where
