-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Condition
  ( Condition (..),

    -- * Smart constructor
    mkCondition,

    -- * Lenses
    cType,
    cKey,
    cValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A JSON string which you can use to limit the event bus permissions you are granting to only accounts that fulfill the condition. Currently, the only supported condition is membership in a certain AWS organization. The string must contain @Type@ , @Key@ , and @Value@ fields. The @Value@ field specifies the ID of the AWS organization. Following is an example value for @Condition@ :
--
-- @'{"Type" : "StringEquals", "Key": "aws:PrincipalOrgID", "Value": "o-1234567890"}'@
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { type' :: Lude.Text,
    key :: Lude.Text,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- * 'key' - Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
-- * 'type'' - Specifies the type of condition. Currently the only supported value is @StringEquals@ .
-- * 'value' - Specifies the value for the key. Currently, this must be the ID of the organization.
mkCondition ::
  -- | 'type''
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  Condition
mkCondition pType_ pKey_ pValue_ =
  Condition' {type' = pType_, key = pKey_, value = pValue_}

-- | Specifies the type of condition. Currently the only supported value is @StringEquals@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Condition Lude.Text
cType = Lens.lens (type' :: Condition -> Lude.Text) (\s a -> s {type' = a} :: Condition)
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKey :: Lens.Lens' Condition Lude.Text
cKey = Lens.lens (key :: Condition -> Lude.Text) (\s a -> s {key = a} :: Condition)
{-# DEPRECATED cKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Specifies the value for the key. Currently, this must be the ID of the organization.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValue :: Lens.Lens' Condition Lude.Text
cValue = Lens.lens (value :: Condition -> Lude.Text) (\s a -> s {value = a} :: Condition)
{-# DEPRECATED cValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToJSON Condition where
  toJSON Condition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("Value" Lude..= value)
          ]
      )
