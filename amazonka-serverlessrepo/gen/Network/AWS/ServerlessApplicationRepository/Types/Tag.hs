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
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This property corresponds to the /AWS CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/Tag Tag>/
-- Data Type.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | This property corresponds to the content of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/Tag Tag>/
    -- Data Type.
    value :: Core.Text,
    -- | This property corresponds to the content of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/Tag Tag>/
    -- Data Type.
    key :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tag_value' - This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/Tag Tag>/
-- Data Type.
--
-- 'key', 'tag_key' - This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/Tag Tag>/
-- Data Type.
newTag ::
  -- | 'value'
  Core.Text ->
  -- | 'key'
  Core.Text ->
  Tag
newTag pValue_ pKey_ =
  Tag' {value = pValue_, key = pKey_}

-- | This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/Tag Tag>/
-- Data Type.
tag_value :: Lens.Lens' Tag Core.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/Tag Tag>/
-- Data Type.
tag_key :: Lens.Lens' Tag Core.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("value" Core..= value),
            Core.Just ("key" Core..= key)
          ]
      )
