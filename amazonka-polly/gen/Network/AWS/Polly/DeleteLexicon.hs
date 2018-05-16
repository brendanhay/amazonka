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
-- Module      : Network.AWS.Polly.DeleteLexicon
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pronunciation lexicon stored in an AWS Region. A lexicon which has been deleted is not available for speech synthesis, nor is it possible to retrieve it using either the @GetLexicon@ or @ListLexicon@ APIs.
--
--
-- For more information, see <http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
module Network.AWS.Polly.DeleteLexicon
    (
    -- * Creating a Request
      deleteLexicon
    , DeleteLexicon
    -- * Request Lenses
    , dlName

    -- * Destructuring the Response
    , deleteLexiconResponse
    , DeleteLexiconResponse
    -- * Response Lenses
    , dlrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Polly.Types
import Network.AWS.Polly.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLexicon' smart constructor.
newtype DeleteLexicon = DeleteLexicon'
  { _dlName :: Sensitive Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLexicon' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlName' - The name of the lexicon to delete. Must be an existing lexicon in the region.
deleteLexicon
    :: Text -- ^ 'dlName'
    -> DeleteLexicon
deleteLexicon pName_ = DeleteLexicon' {_dlName = _Sensitive # pName_}


-- | The name of the lexicon to delete. Must be an existing lexicon in the region.
dlName :: Lens' DeleteLexicon Text
dlName = lens _dlName (\ s a -> s{_dlName = a}) . _Sensitive

instance AWSRequest DeleteLexicon where
        type Rs DeleteLexicon = DeleteLexiconResponse
        request = delete polly
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteLexiconResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteLexicon where

instance NFData DeleteLexicon where

instance ToHeaders DeleteLexicon where
        toHeaders = const mempty

instance ToPath DeleteLexicon where
        toPath DeleteLexicon'{..}
          = mconcat ["/v1/lexicons/", toBS _dlName]

instance ToQuery DeleteLexicon where
        toQuery = const mempty

-- | /See:/ 'deleteLexiconResponse' smart constructor.
newtype DeleteLexiconResponse = DeleteLexiconResponse'
  { _dlrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLexiconResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsResponseStatus' - -- | The response status code.
deleteLexiconResponse
    :: Int -- ^ 'dlrsResponseStatus'
    -> DeleteLexiconResponse
deleteLexiconResponse pResponseStatus_ =
  DeleteLexiconResponse' {_dlrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dlrsResponseStatus :: Lens' DeleteLexiconResponse Int
dlrsResponseStatus = lens _dlrsResponseStatus (\ s a -> s{_dlrsResponseStatus = a})

instance NFData DeleteLexiconResponse where
