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
-- Module      : Network.AWS.Comprehend.DetectDominantLanguage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the dominant language of the input text. For a list of languages that Amazon Comprehend can detect, see <http://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html Amazon Comprehend Supported Languages> .
--
--
module Network.AWS.Comprehend.DetectDominantLanguage
    (
    -- * Creating a Request
      detectDominantLanguage
    , DetectDominantLanguage
    -- * Request Lenses
    , ddlText

    -- * Destructuring the Response
    , detectDominantLanguageResponse
    , DetectDominantLanguageResponse
    -- * Response Lenses
    , ddlrsLanguages
    , ddlrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectDominantLanguage' smart constructor.
newtype DetectDominantLanguage = DetectDominantLanguage'
  { _ddlText :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectDominantLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlText' - A UTF-8 text string. Each string should contain at least 20 characters and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
detectDominantLanguage
    :: Text -- ^ 'ddlText'
    -> DetectDominantLanguage
detectDominantLanguage pText_ = DetectDominantLanguage' {_ddlText = pText_}


-- | A UTF-8 text string. Each string should contain at least 20 characters and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
ddlText :: Lens' DetectDominantLanguage Text
ddlText = lens _ddlText (\ s a -> s{_ddlText = a})

instance AWSRequest DetectDominantLanguage where
        type Rs DetectDominantLanguage =
             DetectDominantLanguageResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 DetectDominantLanguageResponse' <$>
                   (x .?> "Languages" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DetectDominantLanguage where

instance NFData DetectDominantLanguage where

instance ToHeaders DetectDominantLanguage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.DetectDominantLanguage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectDominantLanguage where
        toJSON DetectDominantLanguage'{..}
          = object (catMaybes [Just ("Text" .= _ddlText)])

instance ToPath DetectDominantLanguage where
        toPath = const "/"

instance ToQuery DetectDominantLanguage where
        toQuery = const mempty

-- | /See:/ 'detectDominantLanguageResponse' smart constructor.
data DetectDominantLanguageResponse = DetectDominantLanguageResponse'
  { _ddlrsLanguages      :: !(Maybe [DominantLanguage])
  , _ddlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectDominantLanguageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlrsLanguages' - The languages that Amazon Comprehend detected in the input text. For each language, the response returns the RFC 5646 language code and the level of confidence that Amazon Comprehend has in the accuracy of its inference. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
--
-- * 'ddlrsResponseStatus' - -- | The response status code.
detectDominantLanguageResponse
    :: Int -- ^ 'ddlrsResponseStatus'
    -> DetectDominantLanguageResponse
detectDominantLanguageResponse pResponseStatus_ =
  DetectDominantLanguageResponse'
    {_ddlrsLanguages = Nothing, _ddlrsResponseStatus = pResponseStatus_}


-- | The languages that Amazon Comprehend detected in the input text. For each language, the response returns the RFC 5646 language code and the level of confidence that Amazon Comprehend has in the accuracy of its inference. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
ddlrsLanguages :: Lens' DetectDominantLanguageResponse [DominantLanguage]
ddlrsLanguages = lens _ddlrsLanguages (\ s a -> s{_ddlrsLanguages = a}) . _Default . _Coerce

-- | -- | The response status code.
ddlrsResponseStatus :: Lens' DetectDominantLanguageResponse Int
ddlrsResponseStatus = lens _ddlrsResponseStatus (\ s a -> s{_ddlrsResponseStatus = a})

instance NFData DetectDominantLanguageResponse where
