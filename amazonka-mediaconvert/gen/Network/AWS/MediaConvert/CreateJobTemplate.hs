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
    , cjtAccelerationSettings
    , cjtCategory
    , cjtQueue
    , cjtStatusUpdateIntervalInSecs
    , cjtDescription
    , cjtTags
    , cjtSettings
    , cjtName

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
  { _cjtAccelerationSettings       :: !(Maybe AccelerationSettings)
  , _cjtCategory                   :: !(Maybe Text)
  , _cjtQueue                      :: !(Maybe Text)
  , _cjtStatusUpdateIntervalInSecs :: !(Maybe Nat)
  , _cjtDescription                :: !(Maybe Text)
  , _cjtTags                       :: !(Maybe (Map Text Text))
  , _cjtSettings                   :: !JobTemplateSettings
  , _cjtName                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjtAccelerationSettings' - This is a beta feature. If you are interested in using this feature please contact AWS customer support.
--
-- * 'cjtCategory' - Optional. A category for the job template you are creating
--
-- * 'cjtQueue' - Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- * 'cjtStatusUpdateIntervalInSecs' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'cjtDescription' - Optional. A description of the job template you are creating.
--
-- * 'cjtTags' - The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- * 'cjtSettings' - Undocumented member.
--
-- * 'cjtName' - The name of the job template you are creating.
createJobTemplate
    :: JobTemplateSettings -- ^ 'cjtSettings'
    -> Text -- ^ 'cjtName'
    -> CreateJobTemplate
createJobTemplate pSettings_ pName_ =
  CreateJobTemplate'
    { _cjtAccelerationSettings = Nothing
    , _cjtCategory = Nothing
    , _cjtQueue = Nothing
    , _cjtStatusUpdateIntervalInSecs = Nothing
    , _cjtDescription = Nothing
    , _cjtTags = Nothing
    , _cjtSettings = pSettings_
    , _cjtName = pName_
    }


-- | This is a beta feature. If you are interested in using this feature please contact AWS customer support.
cjtAccelerationSettings :: Lens' CreateJobTemplate (Maybe AccelerationSettings)
cjtAccelerationSettings = lens _cjtAccelerationSettings (\ s a -> s{_cjtAccelerationSettings = a})

-- | Optional. A category for the job template you are creating
cjtCategory :: Lens' CreateJobTemplate (Maybe Text)
cjtCategory = lens _cjtCategory (\ s a -> s{_cjtCategory = a})

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
cjtQueue :: Lens' CreateJobTemplate (Maybe Text)
cjtQueue = lens _cjtQueue (\ s a -> s{_cjtQueue = a})

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
cjtStatusUpdateIntervalInSecs :: Lens' CreateJobTemplate (Maybe Natural)
cjtStatusUpdateIntervalInSecs = lens _cjtStatusUpdateIntervalInSecs (\ s a -> s{_cjtStatusUpdateIntervalInSecs = a}) . mapping _Nat

-- | Optional. A description of the job template you are creating.
cjtDescription :: Lens' CreateJobTemplate (Maybe Text)
cjtDescription = lens _cjtDescription (\ s a -> s{_cjtDescription = a})

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
cjtTags :: Lens' CreateJobTemplate (HashMap Text Text)
cjtTags = lens _cjtTags (\ s a -> s{_cjtTags = a}) . _Default . _Map

-- | Undocumented member.
cjtSettings :: Lens' CreateJobTemplate JobTemplateSettings
cjtSettings = lens _cjtSettings (\ s a -> s{_cjtSettings = a})

-- | The name of the job template you are creating.
cjtName :: Lens' CreateJobTemplate Text
cjtName = lens _cjtName (\ s a -> s{_cjtName = a})

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
                 [("accelerationSettings" .=) <$>
                    _cjtAccelerationSettings,
                  ("category" .=) <$> _cjtCategory,
                  ("queue" .=) <$> _cjtQueue,
                  ("statusUpdateIntervalInSecs" .=) <$>
                    _cjtStatusUpdateIntervalInSecs,
                  ("description" .=) <$> _cjtDescription,
                  ("tags" .=) <$> _cjtTags,
                  Just ("settings" .= _cjtSettings),
                  Just ("name" .= _cjtName)])

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
