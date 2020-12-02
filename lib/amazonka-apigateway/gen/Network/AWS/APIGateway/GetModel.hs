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
-- Module      : Network.AWS.APIGateway.GetModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing model defined for a 'RestApi' resource.
--
--
module Network.AWS.APIGateway.GetModel
    (
    -- * Creating a Request
      getModel
    , GetModel
    -- * Request Lenses
    , ggFlatten
    , ggRestAPIId
    , ggModelName

    -- * Destructuring the Response
    , model
    , Model
    -- * Response Lenses
    , mSchema
    , mName
    , mId
    , mDescription
    , mContentType
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to list information about a model in an existing 'RestApi' resource.
--
--
--
-- /See:/ 'getModel' smart constructor.
data GetModel = GetModel'
  { _ggFlatten   :: !(Maybe Bool)
  , _ggRestAPIId :: !Text
  , _ggModelName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggFlatten' - A query parameter of a Boolean value to resolve (@true@ ) all external model references and returns a flattened model schema or not (@false@ ) The default is @false@ .
--
-- * 'ggRestAPIId' - [Required] The 'RestApi' identifier under which the 'Model' exists.
--
-- * 'ggModelName' - [Required] The name of the model as an identifier.
getModel
    :: Text -- ^ 'ggRestAPIId'
    -> Text -- ^ 'ggModelName'
    -> GetModel
getModel pRestAPIId_ pModelName_ =
  GetModel'
    { _ggFlatten = Nothing
    , _ggRestAPIId = pRestAPIId_
    , _ggModelName = pModelName_
    }


-- | A query parameter of a Boolean value to resolve (@true@ ) all external model references and returns a flattened model schema or not (@false@ ) The default is @false@ .
ggFlatten :: Lens' GetModel (Maybe Bool)
ggFlatten = lens _ggFlatten (\ s a -> s{_ggFlatten = a})

-- | [Required] The 'RestApi' identifier under which the 'Model' exists.
ggRestAPIId :: Lens' GetModel Text
ggRestAPIId = lens _ggRestAPIId (\ s a -> s{_ggRestAPIId = a})

-- | [Required] The name of the model as an identifier.
ggModelName :: Lens' GetModel Text
ggModelName = lens _ggModelName (\ s a -> s{_ggModelName = a})

instance AWSRequest GetModel where
        type Rs GetModel = Model
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetModel where

instance NFData GetModel where

instance ToHeaders GetModel where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetModel where
        toPath GetModel'{..}
          = mconcat
              ["/restapis/", toBS _ggRestAPIId, "/models/",
               toBS _ggModelName]

instance ToQuery GetModel where
        toQuery GetModel'{..}
          = mconcat ["flatten" =: _ggFlatten]
