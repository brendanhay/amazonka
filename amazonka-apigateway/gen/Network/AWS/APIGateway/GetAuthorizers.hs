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
-- Module      : Network.AWS.APIGateway.GetAuthorizers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing < Authorizers> resource.
module Network.AWS.APIGateway.GetAuthorizers
    (
    -- * Creating a Request
      getAuthorizers
    , GetAuthorizers
    -- * Request Lenses
    , gaLimit
    , gaPosition
    , gaRestAPIId

    -- * Destructuring the Response
    , getAuthorizersResponse
    , GetAuthorizersResponse
    -- * Response Lenses
    , garsItems
    , garsPosition
    , garsResponseStatus
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to describe an existing < Authorizers> resource.
--
-- /See:/ 'getAuthorizers' smart constructor.
data GetAuthorizers = GetAuthorizers'
    { _gaLimit     :: !(Maybe Int)
    , _gaPosition  :: !(Maybe Text)
    , _gaRestAPIId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAuthorizers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaLimit'
--
-- * 'gaPosition'
--
-- * 'gaRestAPIId'
getAuthorizers
    :: Text -- ^ 'gaRestAPIId'
    -> GetAuthorizers
getAuthorizers pRestAPIId_ =
    GetAuthorizers'
    { _gaLimit = Nothing
    , _gaPosition = Nothing
    , _gaRestAPIId = pRestAPIId_
    }

-- | Undocumented member.
gaLimit :: Lens' GetAuthorizers (Maybe Int)
gaLimit = lens _gaLimit (\ s a -> s{_gaLimit = a});

-- | Undocumented member.
gaPosition :: Lens' GetAuthorizers (Maybe Text)
gaPosition = lens _gaPosition (\ s a -> s{_gaPosition = a});

-- | The < RestApi> identifier for the < Authorizers> resource.
gaRestAPIId :: Lens' GetAuthorizers Text
gaRestAPIId = lens _gaRestAPIId (\ s a -> s{_gaRestAPIId = a});

instance AWSRequest GetAuthorizers where
        type Rs GetAuthorizers = GetAuthorizersResponse
        request = get aPIGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetAuthorizersResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetAuthorizers

instance ToHeaders GetAuthorizers where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetAuthorizers where
        toPath GetAuthorizers'{..}
          = mconcat
              ["/restapis/", toBS _gaRestAPIId, "/authorizers"]

instance ToQuery GetAuthorizers where
        toQuery GetAuthorizers'{..}
          = mconcat
              ["limit" =: _gaLimit, "position" =: _gaPosition]

-- | Represents a collection of < Authorizer> resources.
--
-- /See:/ 'getAuthorizersResponse' smart constructor.
data GetAuthorizersResponse = GetAuthorizersResponse'
    { _garsItems          :: !(Maybe [Authorizer])
    , _garsPosition       :: !(Maybe Text)
    , _garsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAuthorizersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsItems'
--
-- * 'garsPosition'
--
-- * 'garsResponseStatus'
getAuthorizersResponse
    :: Int -- ^ 'garsResponseStatus'
    -> GetAuthorizersResponse
getAuthorizersResponse pResponseStatus_ =
    GetAuthorizersResponse'
    { _garsItems = Nothing
    , _garsPosition = Nothing
    , _garsResponseStatus = pResponseStatus_
    }

-- | Gets the current list of < Authorizer> resources in the collection.
garsItems :: Lens' GetAuthorizersResponse [Authorizer]
garsItems = lens _garsItems (\ s a -> s{_garsItems = a}) . _Default . _Coerce;

-- | Undocumented member.
garsPosition :: Lens' GetAuthorizersResponse (Maybe Text)
garsPosition = lens _garsPosition (\ s a -> s{_garsPosition = a});

-- | The response status code.
garsResponseStatus :: Lens' GetAuthorizersResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a});
