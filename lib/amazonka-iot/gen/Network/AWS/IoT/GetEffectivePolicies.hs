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
-- Module      : Network.AWS.IoT.GetEffectivePolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets effective policies.
--
--
module Network.AWS.IoT.GetEffectivePolicies
    (
    -- * Creating a Request
      getEffectivePolicies
    , GetEffectivePolicies
    -- * Request Lenses
    , gepPrincipal
    , gepCognitoIdentityPoolId
    , gepThingName

    -- * Destructuring the Response
    , getEffectivePoliciesResponse
    , GetEffectivePoliciesResponse
    -- * Response Lenses
    , geprsEffectivePolicies
    , geprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getEffectivePolicies' smart constructor.
data GetEffectivePolicies = GetEffectivePolicies'
  { _gepPrincipal             :: !(Maybe Text)
  , _gepCognitoIdentityPoolId :: !(Maybe Text)
  , _gepThingName             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEffectivePolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gepPrincipal' - The principal.
--
-- * 'gepCognitoIdentityPoolId' - The Cognito identity pool ID.
--
-- * 'gepThingName' - The thing name.
getEffectivePolicies
    :: GetEffectivePolicies
getEffectivePolicies =
  GetEffectivePolicies'
    { _gepPrincipal = Nothing
    , _gepCognitoIdentityPoolId = Nothing
    , _gepThingName = Nothing
    }


-- | The principal.
gepPrincipal :: Lens' GetEffectivePolicies (Maybe Text)
gepPrincipal = lens _gepPrincipal (\ s a -> s{_gepPrincipal = a})

-- | The Cognito identity pool ID.
gepCognitoIdentityPoolId :: Lens' GetEffectivePolicies (Maybe Text)
gepCognitoIdentityPoolId = lens _gepCognitoIdentityPoolId (\ s a -> s{_gepCognitoIdentityPoolId = a})

-- | The thing name.
gepThingName :: Lens' GetEffectivePolicies (Maybe Text)
gepThingName = lens _gepThingName (\ s a -> s{_gepThingName = a})

instance AWSRequest GetEffectivePolicies where
        type Rs GetEffectivePolicies =
             GetEffectivePoliciesResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetEffectivePoliciesResponse' <$>
                   (x .?> "effectivePolicies" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetEffectivePolicies where

instance NFData GetEffectivePolicies where

instance ToHeaders GetEffectivePolicies where
        toHeaders = const mempty

instance ToJSON GetEffectivePolicies where
        toJSON GetEffectivePolicies'{..}
          = object
              (catMaybes
                 [("principal" .=) <$> _gepPrincipal,
                  ("cognitoIdentityPoolId" .=) <$>
                    _gepCognitoIdentityPoolId])

instance ToPath GetEffectivePolicies where
        toPath = const "/effective-policies"

instance ToQuery GetEffectivePolicies where
        toQuery GetEffectivePolicies'{..}
          = mconcat ["thingName" =: _gepThingName]

-- | /See:/ 'getEffectivePoliciesResponse' smart constructor.
data GetEffectivePoliciesResponse = GetEffectivePoliciesResponse'
  { _geprsEffectivePolicies :: !(Maybe [EffectivePolicy])
  , _geprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEffectivePoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geprsEffectivePolicies' - The effective policies.
--
-- * 'geprsResponseStatus' - -- | The response status code.
getEffectivePoliciesResponse
    :: Int -- ^ 'geprsResponseStatus'
    -> GetEffectivePoliciesResponse
getEffectivePoliciesResponse pResponseStatus_ =
  GetEffectivePoliciesResponse'
    {_geprsEffectivePolicies = Nothing, _geprsResponseStatus = pResponseStatus_}


-- | The effective policies.
geprsEffectivePolicies :: Lens' GetEffectivePoliciesResponse [EffectivePolicy]
geprsEffectivePolicies = lens _geprsEffectivePolicies (\ s a -> s{_geprsEffectivePolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
geprsResponseStatus :: Lens' GetEffectivePoliciesResponse Int
geprsResponseStatus = lens _geprsResponseStatus (\ s a -> s{_geprsResponseStatus = a})

instance NFData GetEffectivePoliciesResponse where
