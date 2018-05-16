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
-- Module      : Network.AWS.MediaConvert.CreateJobTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new job template. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreateJobTemplate
    (
    -- * Creating a Request
      createJobTemplate
    , CreateJobTemplate
    -- * Request Lenses
    , cjtSettings
    , cjtCategory
    , cjtQueue
    , cjtName
    , cjtDescription

    -- * Destructuring the Response
    , createJobTemplateResponse
    , CreateJobTemplateResponse
    -- * Response Lenses
    , cjtrsJobTemplate
    , cjtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJobTemplate' smart constructor.
data CreateJobTemplate = CreateJobTemplate'
  { _cjtSettings    :: !(Maybe JobTemplateSettings)
  , _cjtCategory    :: !(Maybe Text)
  , _cjtQueue       :: !(Maybe Text)
  , _cjtName        :: !(Maybe Text)
  , _cjtDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjtSettings' - Undocumented member.
--
-- * 'cjtCategory' - Optional. A category for the job template you are creating
--
-- * 'cjtQueue' - Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- * 'cjtName' - The name of the job template you are creating.
--
-- * 'cjtDescription' - Optional. A description of the job template you are creating.
createJobTemplate
    :: CreateJobTemplate
createJobTemplate =
  CreateJobTemplate'
    { _cjtSettings = Nothing
    , _cjtCategory = Nothing
    , _cjtQueue = Nothing
    , _cjtName = Nothing
    , _cjtDescription = Nothing
    }


-- | Undocumented member.
cjtSettings :: Lens' CreateJobTemplate (Maybe JobTemplateSettings)
cjtSettings = lens _cjtSettings (\ s a -> s{_cjtSettings = a})

-- | Optional. A category for the job template you are creating
cjtCategory :: Lens' CreateJobTemplate (Maybe Text)
cjtCategory = lens _cjtCategory (\ s a -> s{_cjtCategory = a})

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
cjtQueue :: Lens' CreateJobTemplate (Maybe Text)
cjtQueue = lens _cjtQueue (\ s a -> s{_cjtQueue = a})

-- | The name of the job template you are creating.
cjtName :: Lens' CreateJobTemplate (Maybe Text)
cjtName = lens _cjtName (\ s a -> s{_cjtName = a})

-- | Optional. A description of the job template you are creating.
cjtDescription :: Lens' CreateJobTemplate (Maybe Text)
cjtDescription = lens _cjtDescription (\ s a -> s{_cjtDescription = a})

instance AWSRequest CreateJobTemplate where
        type Rs CreateJobTemplate = CreateJobTemplateResponse
        request = postJSON mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 CreateJobTemplateResponse' <$>
                   (x .?> "jobTemplate") <*> (pure (fromEnum s)))

instance Hashable CreateJobTemplate where

instance NFData CreateJobTemplate where

instance ToHeaders CreateJobTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateJobTemplate where
        toJSON CreateJobTemplate'{..}
          = object
              (catMaybes
                 [("settings" .=) <$> _cjtSettings,
                  ("category" .=) <$> _cjtCategory,
                  ("queue" .=) <$> _cjtQueue, ("name" .=) <$> _cjtName,
                  ("description" .=) <$> _cjtDescription])

instance ToPath CreateJobTemplate where
        toPath = const "/2017-08-29/jobTemplates"

instance ToQuery CreateJobTemplate where
        toQuery = const mempty

-- | /See:/ 'createJobTemplateResponse' smart constructor.
data CreateJobTemplateResponse = CreateJobTemplateResponse'
  { _cjtrsJobTemplate    :: !(Maybe JobTemplate)
  , _cjtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjtrsJobTemplate' - Undocumented member.
--
-- * 'cjtrsResponseStatus' - -- | The response status code.
createJobTemplateResponse
    :: Int -- ^ 'cjtrsResponseStatus'
    -> CreateJobTemplateResponse
createJobTemplateResponse pResponseStatus_ =
  CreateJobTemplateResponse'
    {_cjtrsJobTemplate = Nothing, _cjtrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cjtrsJobTemplate :: Lens' CreateJobTemplateResponse (Maybe JobTemplate)
cjtrsJobTemplate = lens _cjtrsJobTemplate (\ s a -> s{_cjtrsJobTemplate = a})

-- | -- | The response status code.
cjtrsResponseStatus :: Lens' CreateJobTemplateResponse Int
cjtrsResponseStatus = lens _cjtrsResponseStatus (\ s a -> s{_cjtrsResponseStatus = a})

instance NFData CreateJobTemplateResponse where
