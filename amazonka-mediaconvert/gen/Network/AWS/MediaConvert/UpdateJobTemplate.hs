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
-- Module      : Network.AWS.MediaConvert.UpdateJobTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing job templates.
module Network.AWS.MediaConvert.UpdateJobTemplate
    (
    -- * Creating a Request
      updateJobTemplate
    , UpdateJobTemplate
    -- * Request Lenses
    , ujtSettings
    , ujtCategory
    , ujtQueue
    , ujtDescription
    , ujtName

    -- * Destructuring the Response
    , updateJobTemplateResponse
    , UpdateJobTemplateResponse
    -- * Response Lenses
    , ujtrsJobTemplate
    , ujtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateJobTemplate' smart constructor.
data UpdateJobTemplate = UpdateJobTemplate'
  { _ujtSettings    :: !(Maybe JobTemplateSettings)
  , _ujtCategory    :: !(Maybe Text)
  , _ujtQueue       :: !(Maybe Text)
  , _ujtDescription :: !(Maybe Text)
  , _ujtName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujtSettings' - Undocumented member.
--
-- * 'ujtCategory' - The new category for the job template, if you are changing it.
--
-- * 'ujtQueue' - The new queue for the job template, if you are changing it.
--
-- * 'ujtDescription' - The new description for the job template, if you are changing it.
--
-- * 'ujtName' - The name of the job template you are modifying
updateJobTemplate
    :: Text -- ^ 'ujtName'
    -> UpdateJobTemplate
updateJobTemplate pName_ =
  UpdateJobTemplate'
    { _ujtSettings = Nothing
    , _ujtCategory = Nothing
    , _ujtQueue = Nothing
    , _ujtDescription = Nothing
    , _ujtName = pName_
    }


-- | Undocumented member.
ujtSettings :: Lens' UpdateJobTemplate (Maybe JobTemplateSettings)
ujtSettings = lens _ujtSettings (\ s a -> s{_ujtSettings = a})

-- | The new category for the job template, if you are changing it.
ujtCategory :: Lens' UpdateJobTemplate (Maybe Text)
ujtCategory = lens _ujtCategory (\ s a -> s{_ujtCategory = a})

-- | The new queue for the job template, if you are changing it.
ujtQueue :: Lens' UpdateJobTemplate (Maybe Text)
ujtQueue = lens _ujtQueue (\ s a -> s{_ujtQueue = a})

-- | The new description for the job template, if you are changing it.
ujtDescription :: Lens' UpdateJobTemplate (Maybe Text)
ujtDescription = lens _ujtDescription (\ s a -> s{_ujtDescription = a})

-- | The name of the job template you are modifying
ujtName :: Lens' UpdateJobTemplate Text
ujtName = lens _ujtName (\ s a -> s{_ujtName = a})

instance AWSRequest UpdateJobTemplate where
        type Rs UpdateJobTemplate = UpdateJobTemplateResponse
        request = putJSON mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 UpdateJobTemplateResponse' <$>
                   (x .?> "jobTemplate") <*> (pure (fromEnum s)))

instance Hashable UpdateJobTemplate where

instance NFData UpdateJobTemplate where

instance ToHeaders UpdateJobTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateJobTemplate where
        toJSON UpdateJobTemplate'{..}
          = object
              (catMaybes
                 [("settings" .=) <$> _ujtSettings,
                  ("category" .=) <$> _ujtCategory,
                  ("queue" .=) <$> _ujtQueue,
                  ("description" .=) <$> _ujtDescription])

instance ToPath UpdateJobTemplate where
        toPath UpdateJobTemplate'{..}
          = mconcat
              ["/2017-08-29/jobTemplates/", toBS _ujtName]

instance ToQuery UpdateJobTemplate where
        toQuery = const mempty

-- | /See:/ 'updateJobTemplateResponse' smart constructor.
data UpdateJobTemplateResponse = UpdateJobTemplateResponse'
  { _ujtrsJobTemplate    :: !(Maybe JobTemplate)
  , _ujtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJobTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujtrsJobTemplate' - Undocumented member.
--
-- * 'ujtrsResponseStatus' - -- | The response status code.
updateJobTemplateResponse
    :: Int -- ^ 'ujtrsResponseStatus'
    -> UpdateJobTemplateResponse
updateJobTemplateResponse pResponseStatus_ =
  UpdateJobTemplateResponse'
    {_ujtrsJobTemplate = Nothing, _ujtrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ujtrsJobTemplate :: Lens' UpdateJobTemplateResponse (Maybe JobTemplate)
ujtrsJobTemplate = lens _ujtrsJobTemplate (\ s a -> s{_ujtrsJobTemplate = a})

-- | -- | The response status code.
ujtrsResponseStatus :: Lens' UpdateJobTemplateResponse Int
ujtrsResponseStatus = lens _ujtrsResponseStatus (\ s a -> s{_ujtrsResponseStatus = a})

instance NFData UpdateJobTemplateResponse where
