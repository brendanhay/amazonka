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
-- Module      : Network.AWS.Polly.PutLexicon
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a pronunciation lexicon in an AWS Region. If a lexicon with the same name already exists in the region, it is overwritten by the new lexicon. Lexicon operations have eventual consistency, therefore, it might take some time before the lexicon is available to the SynthesizeSpeech operation.
--
--
-- For more information, see <http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
module Network.AWS.Polly.PutLexicon
    (
    -- * Creating a Request
      putLexicon
    , PutLexicon
    -- * Request Lenses
    , plName
    , plContent

    -- * Destructuring the Response
    , putLexiconResponse
    , PutLexiconResponse
    -- * Response Lenses
    , plrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Polly.Types
import Network.AWS.Polly.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putLexicon' smart constructor.
data PutLexicon = PutLexicon'
  { _plName    :: !(Sensitive Text)
  , _plContent :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLexicon' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plName' - Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long.
--
-- * 'plContent' - Content of the PLS lexicon as string data.
putLexicon
    :: Text -- ^ 'plName'
    -> Text -- ^ 'plContent'
    -> PutLexicon
putLexicon pName_ pContent_ =
  PutLexicon' {_plName = _Sensitive # pName_, _plContent = pContent_}


-- | Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long.
plName :: Lens' PutLexicon Text
plName = lens _plName (\ s a -> s{_plName = a}) . _Sensitive

-- | Content of the PLS lexicon as string data.
plContent :: Lens' PutLexicon Text
plContent = lens _plContent (\ s a -> s{_plContent = a})

instance AWSRequest PutLexicon where
        type Rs PutLexicon = PutLexiconResponse
        request = putJSON polly
        response
          = receiveEmpty
              (\ s h x ->
                 PutLexiconResponse' <$> (pure (fromEnum s)))

instance Hashable PutLexicon where

instance NFData PutLexicon where

instance ToHeaders PutLexicon where
        toHeaders = const mempty

instance ToJSON PutLexicon where
        toJSON PutLexicon'{..}
          = object (catMaybes [Just ("Content" .= _plContent)])

instance ToPath PutLexicon where
        toPath PutLexicon'{..}
          = mconcat ["/v1/lexicons/", toBS _plName]

instance ToQuery PutLexicon where
        toQuery = const mempty

-- | /See:/ 'putLexiconResponse' smart constructor.
newtype PutLexiconResponse = PutLexiconResponse'
  { _plrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLexiconResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plrsResponseStatus' - -- | The response status code.
putLexiconResponse
    :: Int -- ^ 'plrsResponseStatus'
    -> PutLexiconResponse
putLexiconResponse pResponseStatus_ =
  PutLexiconResponse' {_plrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
plrsResponseStatus :: Lens' PutLexiconResponse Int
plrsResponseStatus = lens _plrsResponseStatus (\ s a -> s{_plrsResponseStatus = a})

instance NFData PutLexiconResponse where
