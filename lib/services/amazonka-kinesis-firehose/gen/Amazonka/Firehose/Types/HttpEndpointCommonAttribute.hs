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
-- Module      : Amazonka.Firehose.Types.HttpEndpointCommonAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.HttpEndpointCommonAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the metadata that\'s delivered to the specified HTTP endpoint
-- destination.
--
-- /See:/ 'newHttpEndpointCommonAttribute' smart constructor.
data HttpEndpointCommonAttribute = HttpEndpointCommonAttribute'
  { -- | The name of the HTTP endpoint common attribute.
    attributeName :: Data.Sensitive Prelude.Text,
    -- | The value of the HTTP endpoint common attribute.
    attributeValue :: Data.Sensitive Prelude.Text
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
          Data._Sensitive Lens.# pAttributeName_,
        attributeValue =
          Data._Sensitive Lens.# pAttributeValue_
      }

-- | The name of the HTTP endpoint common attribute.
httpEndpointCommonAttribute_attributeName :: Lens.Lens' HttpEndpointCommonAttribute Prelude.Text
httpEndpointCommonAttribute_attributeName = Lens.lens (\HttpEndpointCommonAttribute' {attributeName} -> attributeName) (\s@HttpEndpointCommonAttribute' {} a -> s {attributeName = a} :: HttpEndpointCommonAttribute) Prelude.. Data._Sensitive

-- | The value of the HTTP endpoint common attribute.
httpEndpointCommonAttribute_attributeValue :: Lens.Lens' HttpEndpointCommonAttribute Prelude.Text
httpEndpointCommonAttribute_attributeValue = Lens.lens (\HttpEndpointCommonAttribute' {attributeValue} -> attributeValue) (\s@HttpEndpointCommonAttribute' {} a -> s {attributeValue = a} :: HttpEndpointCommonAttribute) Prelude.. Data._Sensitive

instance Data.FromJSON HttpEndpointCommonAttribute where
  parseJSON =
    Data.withObject
      "HttpEndpointCommonAttribute"
      ( \x ->
          HttpEndpointCommonAttribute'
            Prelude.<$> (x Data..: "AttributeName")
            Prelude.<*> (x Data..: "AttributeValue")
      )

instance Prelude.Hashable HttpEndpointCommonAttribute where
  hashWithSalt _salt HttpEndpointCommonAttribute' {..} =
    _salt `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` attributeValue

instance Prelude.NFData HttpEndpointCommonAttribute where
  rnf HttpEndpointCommonAttribute' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf attributeValue

instance Data.ToJSON HttpEndpointCommonAttribute where
  toJSON HttpEndpointCommonAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Data..= attributeName),
            Prelude.Just
              ("AttributeValue" Data..= attributeValue)
          ]
      )
