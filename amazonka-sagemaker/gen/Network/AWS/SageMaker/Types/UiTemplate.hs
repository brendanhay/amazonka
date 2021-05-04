{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiTemplate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Liquid template for the worker user interface.
--
-- /See:/ 'newUiTemplate' smart constructor.
data UiTemplate = UiTemplate'
  { -- | The content of the Liquid template for the worker user interface.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UiTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'uiTemplate_content' - The content of the Liquid template for the worker user interface.
newUiTemplate ::
  -- | 'content'
  Prelude.Text ->
  UiTemplate
newUiTemplate pContent_ =
  UiTemplate' {content = pContent_}

-- | The content of the Liquid template for the worker user interface.
uiTemplate_content :: Lens.Lens' UiTemplate Prelude.Text
uiTemplate_content = Lens.lens (\UiTemplate' {content} -> content) (\s@UiTemplate' {} a -> s {content = a} :: UiTemplate)

instance Prelude.Hashable UiTemplate

instance Prelude.NFData UiTemplate

instance Prelude.ToJSON UiTemplate where
  toJSON UiTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Content" Prelude..= content)]
      )
