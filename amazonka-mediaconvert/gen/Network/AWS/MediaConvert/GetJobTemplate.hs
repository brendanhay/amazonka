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
-- Module      : Network.AWS.MediaConvert.GetJobTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific job template.
module Network.AWS.MediaConvert.GetJobTemplate
    (
    -- * Creating a Request
      getJobTemplate
    , GetJobTemplate
    -- * Request Lenses
    , gjtName

    -- * Destructuring the Response
    , getJobTemplateResponse
    , GetJobTemplateResponse
    -- * Response Lenses
    , gjtrsJobTemplate
    , gjtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJobTemplate' smart constructor.
newtype GetJobTemplate = GetJobTemplate'
  { _gjtName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjtName' - The name of the job template.
getJobTemplate
    :: Text -- ^ 'gjtName'
    -> GetJobTemplate
getJobTemplate pName_ = GetJobTemplate' {_gjtName = pName_}


-- | The name of the job template.
gjtName :: Lens' GetJobTemplate Text
gjtName = lens _gjtName (\ s a -> s{_gjtName = a})

instance AWSRequest GetJobTemplate where
        type Rs GetJobTemplate = GetJobTemplateResponse
        request = get mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 GetJobTemplateResponse' <$>
                   (x .?> "jobTemplate") <*> (pure (fromEnum s)))

instance Hashable GetJobTemplate where

instance NFData GetJobTemplate where

instance ToHeaders GetJobTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetJobTemplate where
        toPath GetJobTemplate'{..}
          = mconcat
              ["/2017-08-29/jobTemplates/", toBS _gjtName]

instance ToQuery GetJobTemplate where
        toQuery = const mempty

-- | /See:/ 'getJobTemplateResponse' smart constructor.
data GetJobTemplateResponse = GetJobTemplateResponse'
  { _gjtrsJobTemplate    :: !(Maybe JobTemplate)
  , _gjtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjtrsJobTemplate' - Undocumented member.
--
-- * 'gjtrsResponseStatus' - -- | The response status code.
getJobTemplateResponse
    :: Int -- ^ 'gjtrsResponseStatus'
    -> GetJobTemplateResponse
getJobTemplateResponse pResponseStatus_ =
  GetJobTemplateResponse'
    {_gjtrsJobTemplate = Nothing, _gjtrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
gjtrsJobTemplate :: Lens' GetJobTemplateResponse (Maybe JobTemplate)
gjtrsJobTemplate = lens _gjtrsJobTemplate (\ s a -> s{_gjtrsJobTemplate = a})

-- | -- | The response status code.
gjtrsResponseStatus :: Lens' GetJobTemplateResponse Int
gjtrsResponseStatus = lens _gjtrsResponseStatus (\ s a -> s{_gjtrsResponseStatus = a})

instance NFData GetJobTemplateResponse where
