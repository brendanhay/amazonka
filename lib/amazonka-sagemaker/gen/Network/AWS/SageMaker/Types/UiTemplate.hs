{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiTemplate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Liquid template for the worker user interface.
--
--
--
-- /See:/ 'uiTemplate' smart constructor.
newtype UiTemplate = UiTemplate' {_utContent :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UiTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utContent' - The content of the Liquid template for the worker user interface.
uiTemplate ::
  -- | 'utContent'
  Text ->
  UiTemplate
uiTemplate pContent_ = UiTemplate' {_utContent = pContent_}

-- | The content of the Liquid template for the worker user interface.
utContent :: Lens' UiTemplate Text
utContent = lens _utContent (\s a -> s {_utContent = a})

instance Hashable UiTemplate

instance NFData UiTemplate

instance ToJSON UiTemplate where
  toJSON UiTemplate' {..} =
    object (catMaybes [Just ("Content" .= _utContent)])
