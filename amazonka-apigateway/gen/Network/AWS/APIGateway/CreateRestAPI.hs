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
-- Module      : Network.AWS.APIGateway.CreateRestAPI
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new RestApi resource.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/CreateRestAPI.html AWS API Reference> for CreateRestAPI.
module Network.AWS.APIGateway.CreateRestAPI
    (
    -- * Creating a Request
      createRestAPI
    , CreateRestAPI
    -- * Request Lenses
    , craCloneFrom
    , craDescription
    , craName

    -- * Destructuring the Response
    , restAPI
    , RestAPI
    -- * Response Lenses
    , raCreatedDate
    , raName
    , raId
    , raDescription
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to add a new RestApi resource to your collection.
--
-- /See:/ 'createRestAPI' smart constructor.
data CreateRestAPI = CreateRestAPI'
    { _craCloneFrom   :: !(Maybe Text)
    , _craDescription :: !(Maybe Text)
    , _craName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'craCloneFrom'
--
-- * 'craDescription'
--
-- * 'craName'
createRestAPI
    :: Text -- ^ 'craName'
    -> CreateRestAPI
createRestAPI pName_ =
    CreateRestAPI'
    { _craCloneFrom = Nothing
    , _craDescription = Nothing
    , _craName = pName_
    }

-- | The name of the RestApi that you want to clone from.
craCloneFrom :: Lens' CreateRestAPI (Maybe Text)
craCloneFrom = lens _craCloneFrom (\ s a -> s{_craCloneFrom = a});

-- | The description of the RestApi.
craDescription :: Lens' CreateRestAPI (Maybe Text)
craDescription = lens _craDescription (\ s a -> s{_craDescription = a});

-- | The name of the RestApi.
craName :: Lens' CreateRestAPI Text
craName = lens _craName (\ s a -> s{_craName = a});

instance AWSRequest CreateRestAPI where
        type Rs CreateRestAPI = RestAPI
        request = postJSON aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateRestAPI where
        toHeaders = const mempty

instance ToJSON CreateRestAPI where
        toJSON CreateRestAPI'{..}
          = object
              (catMaybes
                 [("cloneFrom" .=) <$> _craCloneFrom,
                  ("description" .=) <$> _craDescription,
                  Just ("name" .= _craName)])

instance ToPath CreateRestAPI where
        toPath = const "/restapis"

instance ToQuery CreateRestAPI where
        toQuery = const mempty
