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
-- Module      : Network.AWS.APIGateway.CreateModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Model' resource to an existing 'RestApi' resource.
--
--
module Network.AWS.APIGateway.CreateModel
    (
    -- * Creating a Request
      createModel
    , CreateModel
    -- * Request Lenses
    , cmSchema
    , cmDescription
    , cmRestAPIId
    , cmName
    , cmContentType

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

-- | Request to add a new 'Model' to an existing 'RestApi' resource.
--
--
--
-- /See:/ 'createModel' smart constructor.
data CreateModel = CreateModel'
  { _cmSchema      :: !(Maybe Text)
  , _cmDescription :: !(Maybe Text)
  , _cmRestAPIId   :: !Text
  , _cmName        :: !Text
  , _cmContentType :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmSchema' - The schema for the model. For @application/json@ models, this should be <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4> model.
--
-- * 'cmDescription' - The description of the model.
--
-- * 'cmRestAPIId' - [Required] The 'RestApi' identifier under which the 'Model' will be created.
--
-- * 'cmName' - [Required] The name of the model. Must be alphanumeric.
--
-- * 'cmContentType' - [Required] The content-type for the model.
createModel
    :: Text -- ^ 'cmRestAPIId'
    -> Text -- ^ 'cmName'
    -> Text -- ^ 'cmContentType'
    -> CreateModel
createModel pRestAPIId_ pName_ pContentType_ =
  CreateModel'
    { _cmSchema = Nothing
    , _cmDescription = Nothing
    , _cmRestAPIId = pRestAPIId_
    , _cmName = pName_
    , _cmContentType = pContentType_
    }


-- | The schema for the model. For @application/json@ models, this should be <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4> model.
cmSchema :: Lens' CreateModel (Maybe Text)
cmSchema = lens _cmSchema (\ s a -> s{_cmSchema = a})

-- | The description of the model.
cmDescription :: Lens' CreateModel (Maybe Text)
cmDescription = lens _cmDescription (\ s a -> s{_cmDescription = a})

-- | [Required] The 'RestApi' identifier under which the 'Model' will be created.
cmRestAPIId :: Lens' CreateModel Text
cmRestAPIId = lens _cmRestAPIId (\ s a -> s{_cmRestAPIId = a})

-- | [Required] The name of the model. Must be alphanumeric.
cmName :: Lens' CreateModel Text
cmName = lens _cmName (\ s a -> s{_cmName = a})

-- | [Required] The content-type for the model.
cmContentType :: Lens' CreateModel Text
cmContentType = lens _cmContentType (\ s a -> s{_cmContentType = a})

instance AWSRequest CreateModel where
        type Rs CreateModel = Model
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateModel where

instance NFData CreateModel where

instance ToHeaders CreateModel where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateModel where
        toJSON CreateModel'{..}
          = object
              (catMaybes
                 [("schema" .=) <$> _cmSchema,
                  ("description" .=) <$> _cmDescription,
                  Just ("name" .= _cmName),
                  Just ("contentType" .= _cmContentType)])

instance ToPath CreateModel where
        toPath CreateModel'{..}
          = mconcat
              ["/restapis/", toBS _cmRestAPIId, "/models"]

instance ToQuery CreateModel where
        toQuery = const mempty
