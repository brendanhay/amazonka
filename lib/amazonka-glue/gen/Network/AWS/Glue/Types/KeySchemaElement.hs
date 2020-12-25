{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.KeySchemaElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.KeySchemaElement
  ( KeySchemaElement (..),

    -- * Smart constructor
    mkKeySchemaElement,

    -- * Lenses
    kseName,
    kseType,
  )
where

import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A partition key pair consisting of a name and a type.
--
-- /See:/ 'mkKeySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { -- | The name of a partition key.
    name :: Types.NameString,
    -- | The type of a partition key.
    type' :: Types.Type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeySchemaElement' value with any optional fields omitted.
mkKeySchemaElement ::
  -- | 'name'
  Types.NameString ->
  -- | 'type\''
  Types.Type ->
  KeySchemaElement
mkKeySchemaElement name type' = KeySchemaElement' {name, type'}

-- | The name of a partition key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kseName :: Lens.Lens' KeySchemaElement Types.NameString
kseName = Lens.field @"name"
{-# DEPRECATED kseName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of a partition key.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kseType :: Lens.Lens' KeySchemaElement Types.Type
kseType = Lens.field @"type'"
{-# DEPRECATED kseType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON KeySchemaElement where
  parseJSON =
    Core.withObject "KeySchemaElement" Core.$
      \x ->
        KeySchemaElement'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..: "Type")
