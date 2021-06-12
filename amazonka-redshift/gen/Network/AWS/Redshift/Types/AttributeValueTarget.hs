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
-- Module      : Network.AWS.Redshift.Types.AttributeValueTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AttributeValueTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes an attribute value.
--
-- /See:/ 'newAttributeValueTarget' smart constructor.
data AttributeValueTarget = AttributeValueTarget'
  { -- | The value of the attribute.
    attributeValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttributeValueTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'attributeValueTarget_attributeValue' - The value of the attribute.
newAttributeValueTarget ::
  AttributeValueTarget
newAttributeValueTarget =
  AttributeValueTarget'
    { attributeValue =
        Core.Nothing
    }

-- | The value of the attribute.
attributeValueTarget_attributeValue :: Lens.Lens' AttributeValueTarget (Core.Maybe Core.Text)
attributeValueTarget_attributeValue = Lens.lens (\AttributeValueTarget' {attributeValue} -> attributeValue) (\s@AttributeValueTarget' {} a -> s {attributeValue = a} :: AttributeValueTarget)

instance Core.FromXML AttributeValueTarget where
  parseXML x =
    AttributeValueTarget'
      Core.<$> (x Core..@? "AttributeValue")

instance Core.Hashable AttributeValueTarget

instance Core.NFData AttributeValueTarget
