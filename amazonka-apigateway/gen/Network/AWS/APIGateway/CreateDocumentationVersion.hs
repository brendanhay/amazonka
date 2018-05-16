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
-- Module      : Network.AWS.APIGateway.CreateDocumentationVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.CreateDocumentationVersion
    (
    -- * Creating a Request
      createDocumentationVersion
    , CreateDocumentationVersion
    -- * Request Lenses
    , cdvStageName
    , cdvDescription
    , cdvRestAPIId
    , cdvDocumentationVersion

    -- * Destructuring the Response
    , documentationVersion
    , DocumentationVersion
    -- * Response Lenses
    , dvCreatedDate
    , dvVersion
    , dvDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Creates a new documentation version of a given API.
--
--
--
-- /See:/ 'createDocumentationVersion' smart constructor.
data CreateDocumentationVersion = CreateDocumentationVersion'
  { _cdvStageName            :: !(Maybe Text)
  , _cdvDescription          :: !(Maybe Text)
  , _cdvRestAPIId            :: !Text
  , _cdvDocumentationVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDocumentationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdvStageName' - The stage name to be associated with the new documentation snapshot.
--
-- * 'cdvDescription' - A description about the new documentation snapshot.
--
-- * 'cdvRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'cdvDocumentationVersion' - [Required] The version identifier of the new snapshot.
createDocumentationVersion
    :: Text -- ^ 'cdvRestAPIId'
    -> Text -- ^ 'cdvDocumentationVersion'
    -> CreateDocumentationVersion
createDocumentationVersion pRestAPIId_ pDocumentationVersion_ =
  CreateDocumentationVersion'
    { _cdvStageName = Nothing
    , _cdvDescription = Nothing
    , _cdvRestAPIId = pRestAPIId_
    , _cdvDocumentationVersion = pDocumentationVersion_
    }


-- | The stage name to be associated with the new documentation snapshot.
cdvStageName :: Lens' CreateDocumentationVersion (Maybe Text)
cdvStageName = lens _cdvStageName (\ s a -> s{_cdvStageName = a})

-- | A description about the new documentation snapshot.
cdvDescription :: Lens' CreateDocumentationVersion (Maybe Text)
cdvDescription = lens _cdvDescription (\ s a -> s{_cdvDescription = a})

-- | [Required] The string identifier of the associated 'RestApi' .
cdvRestAPIId :: Lens' CreateDocumentationVersion Text
cdvRestAPIId = lens _cdvRestAPIId (\ s a -> s{_cdvRestAPIId = a})

-- | [Required] The version identifier of the new snapshot.
cdvDocumentationVersion :: Lens' CreateDocumentationVersion Text
cdvDocumentationVersion = lens _cdvDocumentationVersion (\ s a -> s{_cdvDocumentationVersion = a})

instance AWSRequest CreateDocumentationVersion where
        type Rs CreateDocumentationVersion =
             DocumentationVersion
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateDocumentationVersion where

instance NFData CreateDocumentationVersion where

instance ToHeaders CreateDocumentationVersion where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateDocumentationVersion where
        toJSON CreateDocumentationVersion'{..}
          = object
              (catMaybes
                 [("stageName" .=) <$> _cdvStageName,
                  ("description" .=) <$> _cdvDescription,
                  Just
                    ("documentationVersion" .=
                       _cdvDocumentationVersion)])

instance ToPath CreateDocumentationVersion where
        toPath CreateDocumentationVersion'{..}
          = mconcat
              ["/restapis/", toBS _cdvRestAPIId,
               "/documentation/versions"]

instance ToQuery CreateDocumentationVersion where
        toQuery = const mempty
