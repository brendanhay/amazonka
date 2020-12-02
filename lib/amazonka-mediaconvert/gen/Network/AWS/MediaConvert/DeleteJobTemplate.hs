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
-- Module      : Network.AWS.MediaConvert.DeleteJobTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a job template you have created.
module Network.AWS.MediaConvert.DeleteJobTemplate
    (
    -- * Creating a Request
      deleteJobTemplate
    , DeleteJobTemplate
    -- * Request Lenses
    , djtName

    -- * Destructuring the Response
    , deleteJobTemplateResponse
    , DeleteJobTemplateResponse
    -- * Response Lenses
    , djtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteJobTemplate' smart constructor.
newtype DeleteJobTemplate = DeleteJobTemplate'
  { _djtName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteJobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djtName' - The name of the job template to be deleted.
deleteJobTemplate
    :: Text -- ^ 'djtName'
    -> DeleteJobTemplate
deleteJobTemplate pName_ = DeleteJobTemplate' {_djtName = pName_}


-- | The name of the job template to be deleted.
djtName :: Lens' DeleteJobTemplate Text
djtName = lens _djtName (\ s a -> s{_djtName = a})

instance AWSRequest DeleteJobTemplate where
        type Rs DeleteJobTemplate = DeleteJobTemplateResponse
        request = delete mediaConvert
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteJobTemplateResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteJobTemplate where

instance NFData DeleteJobTemplate where

instance ToHeaders DeleteJobTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteJobTemplate where
        toPath DeleteJobTemplate'{..}
          = mconcat
              ["/2017-08-29/jobTemplates/", toBS _djtName]

instance ToQuery DeleteJobTemplate where
        toQuery = const mempty

-- | /See:/ 'deleteJobTemplateResponse' smart constructor.
newtype DeleteJobTemplateResponse = DeleteJobTemplateResponse'
  { _djtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteJobTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djtrsResponseStatus' - -- | The response status code.
deleteJobTemplateResponse
    :: Int -- ^ 'djtrsResponseStatus'
    -> DeleteJobTemplateResponse
deleteJobTemplateResponse pResponseStatus_ =
  DeleteJobTemplateResponse' {_djtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
djtrsResponseStatus :: Lens' DeleteJobTemplateResponse Int
djtrsResponseStatus = lens _djtrsResponseStatus (\ s a -> s{_djtrsResponseStatus = a})

instance NFData DeleteJobTemplateResponse where
