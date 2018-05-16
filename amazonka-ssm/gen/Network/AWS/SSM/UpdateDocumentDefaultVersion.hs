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
-- Module      : Network.AWS.SSM.UpdateDocumentDefaultVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the default version of a document.
--
--
module Network.AWS.SSM.UpdateDocumentDefaultVersion
    (
    -- * Creating a Request
      updateDocumentDefaultVersion
    , UpdateDocumentDefaultVersion
    -- * Request Lenses
    , uddvName
    , uddvDocumentVersion

    -- * Destructuring the Response
    , updateDocumentDefaultVersionResponse
    , UpdateDocumentDefaultVersionResponse
    -- * Response Lenses
    , uddvrsDescription
    , uddvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'updateDocumentDefaultVersion' smart constructor.
data UpdateDocumentDefaultVersion = UpdateDocumentDefaultVersion'
  { _uddvName            :: !Text
  , _uddvDocumentVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocumentDefaultVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uddvName' - The name of a custom document that you want to set as the default version.
--
-- * 'uddvDocumentVersion' - The version of a custom document that you want to set as the default version.
updateDocumentDefaultVersion
    :: Text -- ^ 'uddvName'
    -> Text -- ^ 'uddvDocumentVersion'
    -> UpdateDocumentDefaultVersion
updateDocumentDefaultVersion pName_ pDocumentVersion_ =
  UpdateDocumentDefaultVersion'
    {_uddvName = pName_, _uddvDocumentVersion = pDocumentVersion_}


-- | The name of a custom document that you want to set as the default version.
uddvName :: Lens' UpdateDocumentDefaultVersion Text
uddvName = lens _uddvName (\ s a -> s{_uddvName = a})

-- | The version of a custom document that you want to set as the default version.
uddvDocumentVersion :: Lens' UpdateDocumentDefaultVersion Text
uddvDocumentVersion = lens _uddvDocumentVersion (\ s a -> s{_uddvDocumentVersion = a})

instance AWSRequest UpdateDocumentDefaultVersion
         where
        type Rs UpdateDocumentDefaultVersion =
             UpdateDocumentDefaultVersionResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDocumentDefaultVersionResponse' <$>
                   (x .?> "Description") <*> (pure (fromEnum s)))

instance Hashable UpdateDocumentDefaultVersion where

instance NFData UpdateDocumentDefaultVersion where

instance ToHeaders UpdateDocumentDefaultVersion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdateDocumentDefaultVersion" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDocumentDefaultVersion where
        toJSON UpdateDocumentDefaultVersion'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _uddvName),
                  Just ("DocumentVersion" .= _uddvDocumentVersion)])

instance ToPath UpdateDocumentDefaultVersion where
        toPath = const "/"

instance ToQuery UpdateDocumentDefaultVersion where
        toQuery = const mempty

-- | /See:/ 'updateDocumentDefaultVersionResponse' smart constructor.
data UpdateDocumentDefaultVersionResponse = UpdateDocumentDefaultVersionResponse'
  { _uddvrsDescription    :: !(Maybe DocumentDefaultVersionDescription)
  , _uddvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocumentDefaultVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uddvrsDescription' - The description of a custom document that you want to set as the default version.
--
-- * 'uddvrsResponseStatus' - -- | The response status code.
updateDocumentDefaultVersionResponse
    :: Int -- ^ 'uddvrsResponseStatus'
    -> UpdateDocumentDefaultVersionResponse
updateDocumentDefaultVersionResponse pResponseStatus_ =
  UpdateDocumentDefaultVersionResponse'
    {_uddvrsDescription = Nothing, _uddvrsResponseStatus = pResponseStatus_}


-- | The description of a custom document that you want to set as the default version.
uddvrsDescription :: Lens' UpdateDocumentDefaultVersionResponse (Maybe DocumentDefaultVersionDescription)
uddvrsDescription = lens _uddvrsDescription (\ s a -> s{_uddvrsDescription = a})

-- | -- | The response status code.
uddvrsResponseStatus :: Lens' UpdateDocumentDefaultVersionResponse Int
uddvrsResponseStatus = lens _uddvrsResponseStatus (\ s a -> s{_uddvrsResponseStatus = a})

instance NFData UpdateDocumentDefaultVersionResponse
         where
