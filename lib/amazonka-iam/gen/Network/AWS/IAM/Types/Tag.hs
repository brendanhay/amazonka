{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tKey
  , tValue
  ) where

import qualified Network.AWS.IAM.Types.Key as Types
import qualified Network.AWS.IAM.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that represents user-provided metadata that can be associated with a resource such as an IAM user or role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { key :: Types.Key
    -- ^ The key name that can be used to look up or retrieve the associated value. For example, @Department@ or @Cost Center@ are common choices.
  , value :: Types.Value
    -- ^ The value associated with this tag. For example, tags with a key name of @Department@ could have values such as @Human Resources@ , @Accounting@ , and @Support@ . Tags with a key name of @Cost Center@ might have values that consist of the number associated with the different cost centers in your company. Typically, many resources have tags with the same key name but with different values.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Types.Key -- ^ 'key'
    -> Types.Value -- ^ 'value'
    -> Tag
mkTag key value = Tag'{key, value}

-- | The key name that can be used to look up or retrieve the associated value. For example, @Department@ or @Cost Center@ are common choices.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# INLINEABLE tKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value associated with this tag. For example, tags with a key name of @Department@ could have values such as @Human Resources@ , @Accounting@ , and @Support@ . Tags with a key name of @Cost Center@ might have values that consist of the number associated with the different cost centers in your company. Typically, many resources have tags with the same key name but with different values.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Types.Value
tValue = Lens.field @"value"
{-# INLINEABLE tValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery Tag where
        toQuery Tag{..}
          = Core.toQueryPair "Key" key Core.<> Core.toQueryPair "Value" value

instance Core.FromXML Tag where
        parseXML x
          = Tag' Core.<$> (x Core..@ "Key") Core.<*> x Core..@ "Value"
