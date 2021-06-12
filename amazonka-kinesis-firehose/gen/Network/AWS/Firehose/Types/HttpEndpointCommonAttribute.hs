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
-- Module      : Network.AWS.Firehose.Types.HttpEndpointCommonAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HttpEndpointCommonAttribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the metadata that\'s delivered to the specified HTTP endpoint
-- destination.
--
-- /See:/ 'newHttpEndpointCommonAttribute' smart constructor.
data HttpEndpointCommonAttribute = HttpEndpointCommonAttribute'
  { -- | The name of the HTTP endpoint common attribute.
    attributeName :: Core.Sensitive Core.Text,
    -- | The value of the HTTP endpoint common attribute.
    attributeValue :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpEndpointCommonAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'httpEndpointCommonAttribute_attributeName' - The name of the HTTP endpoint common attribute.
--
-- 'attributeValue', 'httpEndpointCommonAttribute_attributeValue' - The value of the HTTP endpoint common attribute.
newHttpEndpointCommonAttribute ::
  -- | 'attributeName'
  Core.Text ->
  -- | 'attributeValue'
  Core.Text ->
  HttpEndpointCommonAttribute
newHttpEndpointCommonAttribute
  pAttributeName_
  pAttributeValue_ =
    HttpEndpointCommonAttribute'
      { attributeName =
          Core._Sensitive Lens.# pAttributeName_,
        attributeValue =
          Core._Sensitive Lens.# pAttributeValue_
      }

-- | The name of the HTTP endpoint common attribute.
httpEndpointCommonAttribute_attributeName :: Lens.Lens' HttpEndpointCommonAttribute Core.Text
httpEndpointCommonAttribute_attributeName = Lens.lens (\HttpEndpointCommonAttribute' {attributeName} -> attributeName) (\s@HttpEndpointCommonAttribute' {} a -> s {attributeName = a} :: HttpEndpointCommonAttribute) Core.. Core._Sensitive

-- | The value of the HTTP endpoint common attribute.
httpEndpointCommonAttribute_attributeValue :: Lens.Lens' HttpEndpointCommonAttribute Core.Text
httpEndpointCommonAttribute_attributeValue = Lens.lens (\HttpEndpointCommonAttribute' {attributeValue} -> attributeValue) (\s@HttpEndpointCommonAttribute' {} a -> s {attributeValue = a} :: HttpEndpointCommonAttribute) Core.. Core._Sensitive

instance Core.FromJSON HttpEndpointCommonAttribute where
  parseJSON =
    Core.withObject
      "HttpEndpointCommonAttribute"
      ( \x ->
          HttpEndpointCommonAttribute'
            Core.<$> (x Core..: "AttributeName")
            Core.<*> (x Core..: "AttributeValue")
      )

instance Core.Hashable HttpEndpointCommonAttribute

instance Core.NFData HttpEndpointCommonAttribute

instance Core.ToJSON HttpEndpointCommonAttribute where
  toJSON HttpEndpointCommonAttribute' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AttributeName" Core..= attributeName),
            Core.Just ("AttributeValue" Core..= attributeValue)
          ]
      )
