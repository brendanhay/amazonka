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
-- Module      : Network.AWS.CloudTrail.Types.LookupAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.LookupAttribute where

import Network.AWS.CloudTrail.Types.LookupAttributeKey
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies an attribute and value that filter the events returned.
--
-- /See:/ 'newLookupAttribute' smart constructor.
data LookupAttribute = LookupAttribute'
  { -- | Specifies an attribute on which to filter the events returned.
    attributeKey :: LookupAttributeKey,
    -- | Specifies a value for the specified AttributeKey.
    attributeValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LookupAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeKey', 'lookupAttribute_attributeKey' - Specifies an attribute on which to filter the events returned.
--
-- 'attributeValue', 'lookupAttribute_attributeValue' - Specifies a value for the specified AttributeKey.
newLookupAttribute ::
  -- | 'attributeKey'
  LookupAttributeKey ->
  -- | 'attributeValue'
  Core.Text ->
  LookupAttribute
newLookupAttribute pAttributeKey_ pAttributeValue_ =
  LookupAttribute'
    { attributeKey = pAttributeKey_,
      attributeValue = pAttributeValue_
    }

-- | Specifies an attribute on which to filter the events returned.
lookupAttribute_attributeKey :: Lens.Lens' LookupAttribute LookupAttributeKey
lookupAttribute_attributeKey = Lens.lens (\LookupAttribute' {attributeKey} -> attributeKey) (\s@LookupAttribute' {} a -> s {attributeKey = a} :: LookupAttribute)

-- | Specifies a value for the specified AttributeKey.
lookupAttribute_attributeValue :: Lens.Lens' LookupAttribute Core.Text
lookupAttribute_attributeValue = Lens.lens (\LookupAttribute' {attributeValue} -> attributeValue) (\s@LookupAttribute' {} a -> s {attributeValue = a} :: LookupAttribute)

instance Core.Hashable LookupAttribute

instance Core.NFData LookupAttribute

instance Core.ToJSON LookupAttribute where
  toJSON LookupAttribute' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AttributeKey" Core..= attributeKey),
            Core.Just ("AttributeValue" Core..= attributeValue)
          ]
      )
