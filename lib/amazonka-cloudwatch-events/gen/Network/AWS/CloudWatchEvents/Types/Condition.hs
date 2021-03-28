{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.Condition
  ( Condition (..)
  -- * Smart constructor
  , mkCondition
  -- * Lenses
  , cType
  , cKey
  , cValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A JSON string which you can use to limit the event bus permissions you are granting to only accounts that fulfill the condition. Currently, the only supported condition is membership in a certain AWS organization. The string must contain @Type@ , @Key@ , and @Value@ fields. The @Value@ field specifies the ID of the AWS organization. Following is an example value for @Condition@ :
--
-- @'{"Type" : "StringEquals", "Key": "aws:PrincipalOrgID", "Value": "o-1234567890"}'@ 
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { type' :: Core.Text
    -- ^ Specifies the type of condition. Currently the only supported value is @StringEquals@ .
  , key :: Core.Text
    -- ^ Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
  , value :: Core.Text
    -- ^ Specifies the value for the key. Currently, this must be the ID of the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Condition' value with any optional fields omitted.
mkCondition
    :: Core.Text -- ^ 'type\''
    -> Core.Text -- ^ 'key'
    -> Core.Text -- ^ 'value'
    -> Condition
mkCondition type' key value = Condition'{type', key, value}

-- | Specifies the type of condition. Currently the only supported value is @StringEquals@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Condition Core.Text
cType = Lens.field @"type'"
{-# INLINEABLE cType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKey :: Lens.Lens' Condition Core.Text
cKey = Lens.field @"key"
{-# INLINEABLE cKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Specifies the value for the key. Currently, this must be the ID of the organization.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValue :: Lens.Lens' Condition Core.Text
cValue = Lens.field @"value"
{-# INLINEABLE cValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Condition where
        toJSON Condition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'), Core.Just ("Key" Core..= key),
                  Core.Just ("Value" Core..= value)])
