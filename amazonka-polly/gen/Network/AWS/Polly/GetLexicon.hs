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
-- Module      : Network.AWS.Polly.GetLexicon
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the content of the specified pronunciation lexicon stored in an AWS Region. For more information, see <http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
--
module Network.AWS.Polly.GetLexicon
    (
    -- * Creating a Request
      getLexicon
    , GetLexicon
    -- * Request Lenses
    , glName

    -- * Destructuring the Response
    , getLexiconResponse
    , GetLexiconResponse
    -- * Response Lenses
    , glrsLexiconAttributes
    , glrsLexicon
    , glrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Polly.Types
import Network.AWS.Polly.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLexicon' smart constructor.
newtype GetLexicon = GetLexicon'
  { _glName :: Sensitive Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLexicon' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glName' - Name of the lexicon.
getLexicon
    :: Text -- ^ 'glName'
    -> GetLexicon
getLexicon pName_ = GetLexicon' {_glName = _Sensitive # pName_}


-- | Name of the lexicon.
glName :: Lens' GetLexicon Text
glName = lens _glName (\ s a -> s{_glName = a}) . _Sensitive

instance AWSRequest GetLexicon where
        type Rs GetLexicon = GetLexiconResponse
        request = get polly
        response
          = receiveJSON
              (\ s h x ->
                 GetLexiconResponse' <$>
                   (x .?> "LexiconAttributes") <*> (x .?> "Lexicon") <*>
                     (pure (fromEnum s)))

instance Hashable GetLexicon where

instance NFData GetLexicon where

instance ToHeaders GetLexicon where
        toHeaders = const mempty

instance ToPath GetLexicon where
        toPath GetLexicon'{..}
          = mconcat ["/v1/lexicons/", toBS _glName]

instance ToQuery GetLexicon where
        toQuery = const mempty

-- | /See:/ 'getLexiconResponse' smart constructor.
data GetLexiconResponse = GetLexiconResponse'
  { _glrsLexiconAttributes :: !(Maybe LexiconAttributes)
  , _glrsLexicon           :: !(Maybe Lexicon)
  , _glrsResponseStatus    :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLexiconResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glrsLexiconAttributes' - Metadata of the lexicon, including phonetic alphabetic used, language code, lexicon ARN, number of lexemes defined in the lexicon, and size of lexicon in bytes.
--
-- * 'glrsLexicon' - Lexicon object that provides name and the string content of the lexicon.
--
-- * 'glrsResponseStatus' - -- | The response status code.
getLexiconResponse
    :: Int -- ^ 'glrsResponseStatus'
    -> GetLexiconResponse
getLexiconResponse pResponseStatus_ =
  GetLexiconResponse'
    { _glrsLexiconAttributes = Nothing
    , _glrsLexicon = Nothing
    , _glrsResponseStatus = pResponseStatus_
    }


-- | Metadata of the lexicon, including phonetic alphabetic used, language code, lexicon ARN, number of lexemes defined in the lexicon, and size of lexicon in bytes.
glrsLexiconAttributes :: Lens' GetLexiconResponse (Maybe LexiconAttributes)
glrsLexiconAttributes = lens _glrsLexiconAttributes (\ s a -> s{_glrsLexiconAttributes = a})

-- | Lexicon object that provides name and the string content of the lexicon.
glrsLexicon :: Lens' GetLexiconResponse (Maybe Lexicon)
glrsLexicon = lens _glrsLexicon (\ s a -> s{_glrsLexicon = a})

-- | -- | The response status code.
glrsResponseStatus :: Lens' GetLexiconResponse Int
glrsResponseStatus = lens _glrsResponseStatus (\ s a -> s{_glrsResponseStatus = a})

instance NFData GetLexiconResponse where
