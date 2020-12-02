{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.RenderUiTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renders the UI template so that you can preview the worker's experience.
module Network.AWS.SageMaker.RenderUiTemplate
  ( -- * Creating a Request
    renderUiTemplate,
    RenderUiTemplate,

    -- * Request Lenses
    rutUiTemplate,
    rutHumanTaskUiARN,
    rutTask,
    rutRoleARN,

    -- * Destructuring the Response
    renderUiTemplateResponse,
    RenderUiTemplateResponse,

    -- * Response Lenses
    rutrsResponseStatus,
    rutrsRenderedContent,
    rutrsErrors,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'renderUiTemplate' smart constructor.
data RenderUiTemplate = RenderUiTemplate'
  { _rutUiTemplate ::
      !(Maybe UiTemplate),
    _rutHumanTaskUiARN :: !(Maybe Text),
    _rutTask :: !RenderableTask,
    _rutRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RenderUiTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rutUiTemplate' - A @Template@ object containing the worker UI template to render.
--
-- * 'rutHumanTaskUiARN' - The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter. See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
--
-- * 'rutTask' - A @RenderableTask@ object containing a representative task to render.
--
-- * 'rutRoleARN' - The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
renderUiTemplate ::
  -- | 'rutTask'
  RenderableTask ->
  -- | 'rutRoleARN'
  Text ->
  RenderUiTemplate
renderUiTemplate pTask_ pRoleARN_ =
  RenderUiTemplate'
    { _rutUiTemplate = Nothing,
      _rutHumanTaskUiARN = Nothing,
      _rutTask = pTask_,
      _rutRoleARN = pRoleARN_
    }

-- | A @Template@ object containing the worker UI template to render.
rutUiTemplate :: Lens' RenderUiTemplate (Maybe UiTemplate)
rutUiTemplate = lens _rutUiTemplate (\s a -> s {_rutUiTemplate = a})

-- | The @HumanTaskUiArn@ of the worker UI that you want to render. Do not provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter. See a list of available Human Ui Amazon Resource Names (ARNs) in 'UiConfig' .
rutHumanTaskUiARN :: Lens' RenderUiTemplate (Maybe Text)
rutHumanTaskUiARN = lens _rutHumanTaskUiARN (\s a -> s {_rutHumanTaskUiARN = a})

-- | A @RenderableTask@ object containing a representative task to render.
rutTask :: Lens' RenderUiTemplate RenderableTask
rutTask = lens _rutTask (\s a -> s {_rutTask = a})

-- | The Amazon Resource Name (ARN) that has access to the S3 objects that are used by the template.
rutRoleARN :: Lens' RenderUiTemplate Text
rutRoleARN = lens _rutRoleARN (\s a -> s {_rutRoleARN = a})

instance AWSRequest RenderUiTemplate where
  type Rs RenderUiTemplate = RenderUiTemplateResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          RenderUiTemplateResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "RenderedContent")
            <*> (x .?> "Errors" .!@ mempty)
      )

instance Hashable RenderUiTemplate

instance NFData RenderUiTemplate

instance ToHeaders RenderUiTemplate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.RenderUiTemplate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RenderUiTemplate where
  toJSON RenderUiTemplate' {..} =
    object
      ( catMaybes
          [ ("UiTemplate" .=) <$> _rutUiTemplate,
            ("HumanTaskUiArn" .=) <$> _rutHumanTaskUiARN,
            Just ("Task" .= _rutTask),
            Just ("RoleArn" .= _rutRoleARN)
          ]
      )

instance ToPath RenderUiTemplate where
  toPath = const "/"

instance ToQuery RenderUiTemplate where
  toQuery = const mempty

-- | /See:/ 'renderUiTemplateResponse' smart constructor.
data RenderUiTemplateResponse = RenderUiTemplateResponse'
  { _rutrsResponseStatus ::
      !Int,
    _rutrsRenderedContent :: !Text,
    _rutrsErrors :: ![RenderingError]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RenderUiTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rutrsResponseStatus' - -- | The response status code.
--
-- * 'rutrsRenderedContent' - A Liquid template that renders the HTML for the worker UI.
--
-- * 'rutrsErrors' - A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
renderUiTemplateResponse ::
  -- | 'rutrsResponseStatus'
  Int ->
  -- | 'rutrsRenderedContent'
  Text ->
  RenderUiTemplateResponse
renderUiTemplateResponse pResponseStatus_ pRenderedContent_ =
  RenderUiTemplateResponse'
    { _rutrsResponseStatus =
        pResponseStatus_,
      _rutrsRenderedContent = pRenderedContent_,
      _rutrsErrors = mempty
    }

-- | -- | The response status code.
rutrsResponseStatus :: Lens' RenderUiTemplateResponse Int
rutrsResponseStatus = lens _rutrsResponseStatus (\s a -> s {_rutrsResponseStatus = a})

-- | A Liquid template that renders the HTML for the worker UI.
rutrsRenderedContent :: Lens' RenderUiTemplateResponse Text
rutrsRenderedContent = lens _rutrsRenderedContent (\s a -> s {_rutrsRenderedContent = a})

-- | A list of one or more @RenderingError@ objects if any were encountered while rendering the template. If there were no errors, the list is empty.
rutrsErrors :: Lens' RenderUiTemplateResponse [RenderingError]
rutrsErrors = lens _rutrsErrors (\s a -> s {_rutrsErrors = a}) . _Coerce

instance NFData RenderUiTemplateResponse
