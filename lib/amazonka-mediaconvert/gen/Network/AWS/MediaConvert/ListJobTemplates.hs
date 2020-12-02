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
-- Module      : Network.AWS.MediaConvert.ListJobTemplates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your job templates. This will return the templates themselves, not just a list of them. To retrieve the next twenty templates, use the nextToken string returned with the array
module Network.AWS.MediaConvert.ListJobTemplates
    (
    -- * Creating a Request
      listJobTemplates
    , ListJobTemplates
    -- * Request Lenses
    , ljtCategory
    , ljtListBy
    , ljtNextToken
    , ljtOrder
    , ljtMaxResults

    -- * Destructuring the Response
    , listJobTemplatesResponse
    , ListJobTemplatesResponse
    -- * Response Lenses
    , ljtrsJobTemplates
    , ljtrsNextToken
    , ljtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJobTemplates' smart constructor.
data ListJobTemplates = ListJobTemplates'
  { _ljtCategory   :: !(Maybe Text)
  , _ljtListBy     :: !(Maybe JobTemplateListBy)
  , _ljtNextToken  :: !(Maybe Text)
  , _ljtOrder      :: !(Maybe Order)
  , _ljtMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljtCategory' - Optionally, specify a job template category to limit responses to only job templates from that category.
--
-- * 'ljtListBy' - Undocumented member.
--
-- * 'ljtNextToken' - Use this string, provided with the response to a previous request, to request the next batch of job templates.
--
-- * 'ljtOrder' - Undocumented member.
--
-- * 'ljtMaxResults' - Optional. Number of job templates, up to twenty, that will be returned at one time.
listJobTemplates
    :: ListJobTemplates
listJobTemplates =
  ListJobTemplates'
    { _ljtCategory = Nothing
    , _ljtListBy = Nothing
    , _ljtNextToken = Nothing
    , _ljtOrder = Nothing
    , _ljtMaxResults = Nothing
    }


-- | Optionally, specify a job template category to limit responses to only job templates from that category.
ljtCategory :: Lens' ListJobTemplates (Maybe Text)
ljtCategory = lens _ljtCategory (\ s a -> s{_ljtCategory = a})

-- | Undocumented member.
ljtListBy :: Lens' ListJobTemplates (Maybe JobTemplateListBy)
ljtListBy = lens _ljtListBy (\ s a -> s{_ljtListBy = a})

-- | Use this string, provided with the response to a previous request, to request the next batch of job templates.
ljtNextToken :: Lens' ListJobTemplates (Maybe Text)
ljtNextToken = lens _ljtNextToken (\ s a -> s{_ljtNextToken = a})

-- | Undocumented member.
ljtOrder :: Lens' ListJobTemplates (Maybe Order)
ljtOrder = lens _ljtOrder (\ s a -> s{_ljtOrder = a})

-- | Optional. Number of job templates, up to twenty, that will be returned at one time.
ljtMaxResults :: Lens' ListJobTemplates (Maybe Int)
ljtMaxResults = lens _ljtMaxResults (\ s a -> s{_ljtMaxResults = a})

instance AWSRequest ListJobTemplates where
        type Rs ListJobTemplates = ListJobTemplatesResponse
        request = get mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 ListJobTemplatesResponse' <$>
                   (x .?> "jobTemplates" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListJobTemplates where

instance NFData ListJobTemplates where

instance ToHeaders ListJobTemplates where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListJobTemplates where
        toPath = const "/2017-08-29/jobTemplates"

instance ToQuery ListJobTemplates where
        toQuery ListJobTemplates'{..}
          = mconcat
              ["category" =: _ljtCategory, "listBy" =: _ljtListBy,
               "nextToken" =: _ljtNextToken, "order" =: _ljtOrder,
               "maxResults" =: _ljtMaxResults]

-- | /See:/ 'listJobTemplatesResponse' smart constructor.
data ListJobTemplatesResponse = ListJobTemplatesResponse'
  { _ljtrsJobTemplates   :: !(Maybe [JobTemplate])
  , _ljtrsNextToken      :: !(Maybe Text)
  , _ljtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljtrsJobTemplates' - List of Job templates.
--
-- * 'ljtrsNextToken' - Use this string to request the next batch of job templates.
--
-- * 'ljtrsResponseStatus' - -- | The response status code.
listJobTemplatesResponse
    :: Int -- ^ 'ljtrsResponseStatus'
    -> ListJobTemplatesResponse
listJobTemplatesResponse pResponseStatus_ =
  ListJobTemplatesResponse'
    { _ljtrsJobTemplates = Nothing
    , _ljtrsNextToken = Nothing
    , _ljtrsResponseStatus = pResponseStatus_
    }


-- | List of Job templates.
ljtrsJobTemplates :: Lens' ListJobTemplatesResponse [JobTemplate]
ljtrsJobTemplates = lens _ljtrsJobTemplates (\ s a -> s{_ljtrsJobTemplates = a}) . _Default . _Coerce

-- | Use this string to request the next batch of job templates.
ljtrsNextToken :: Lens' ListJobTemplatesResponse (Maybe Text)
ljtrsNextToken = lens _ljtrsNextToken (\ s a -> s{_ljtrsNextToken = a})

-- | -- | The response status code.
ljtrsResponseStatus :: Lens' ListJobTemplatesResponse Int
ljtrsResponseStatus = lens _ljtrsResponseStatus (\ s a -> s{_ljtrsResponseStatus = a})

instance NFData ListJobTemplatesResponse where
