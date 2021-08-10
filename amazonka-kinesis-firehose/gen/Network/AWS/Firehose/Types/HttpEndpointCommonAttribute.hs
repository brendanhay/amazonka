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
import qualified Network.AWS.Prelude as Prelude

-- | Describes the metadata that\'s delivered to the specified HTTP endpoint
-- destination.
--
-- /See:/ 'newHttpEndpointCommonAttribute' smart constructor.
data HttpEndpointCommonAttribute = HttpEndpointCommonAttribute'
  { -- | The name of the HTTP endpoint common attribute.
    attributeName :: Core.Sensitive Prelude.Text,
    -- | The value of the HTTP endpoint common attribute.
    attributeValue :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'attributeValue'
  Prelude.Text ->
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
httpEndpointCommonAttribute_attributeName :: Lens.Lens' HttpEndpointCommonAttribute Prelude.Text
httpEndpointCommonAttribute_attributeName = Lens.lens (\HttpEndpointCommonAttribute' {attributeName} -> attributeName) (\s@HttpEndpointCommonAttribute' {} a -> s {attributeName = a} :: HttpEndpointCommonAttribute) Prelude.. Core._Sensitive

-- | The value of the HTTP endpoint common attribute.
httpEndpointCommonAttribute_attributeValue :: Lens.Lens' HttpEndpointCommonAttribute Prelude.Text
httpEndpointCommonAttribute_attributeValue = Lens.lens (\HttpEndpointCommonAttribute' {attributeValue} -> attributeValue) (\s@HttpEndpointCommonAttribute' {} a -> s {attributeValue = a} :: HttpEndpointCommonAttribute) Prelude.. Core._Sensitive

instance Core.FromJSON HttpEndpointCommonAttribute where
  parseJSON =
    Core.withObject
      "HttpEndpointCommonAttribute"
      ( \x ->
          HttpEndpointCommonAttribute'
            Prelude.<$> (x Core..: "AttributeName")
            Prelude.<*> (x Core..: "AttributeValue")
      )

instance Prelude.Hashable HttpEndpointCommonAttribute

instance Prelude.NFData HttpEndpointCommonAttribute

instance Core.ToJSON HttpEndpointCommonAttribute where
  toJSON HttpEndpointCommonAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Core..= attributeName),
            Prelude.Just
              ("AttributeValue" Core..= attributeValue)
          ]
      )
