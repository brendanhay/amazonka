{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tKey
  , tValue
  ) where

import qualified Network.AWS.CloudFormation.Types.Key as Types
import qualified Network.AWS.CloudFormation.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Tag type enables you to specify a key-value pair that can be used to store information about an AWS CloudFormation stack.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { key :: Types.Key
    -- ^ /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
  , value :: Types.Value
    -- ^ /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Types.Key -- ^ 'key'
    -> Types.Value -- ^ 'value'
    -> Tag
mkTag key value = Tag'{key, value}

-- | /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# INLINEABLE tKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
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
