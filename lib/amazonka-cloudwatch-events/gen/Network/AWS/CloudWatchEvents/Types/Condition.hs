{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cValue,
    cKey,
    cType,
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
  { -- | Specifies the value for the key. Currently, this must be the ID of the organization.
    value :: Lude.Text,
    -- | Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
    key :: Lude.Text,
    -- | Specifies the type of condition. Currently the only supported value is @StringEquals@ .
    type' :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- * 'value' - Specifies the value for the key. Currently, this must be the ID of the organization.
-- * 'key' - Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
-- * 'type'' - Specifies the type of condition. Currently the only supported value is @StringEquals@ .
mkCondition ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  Condition
mkCondition pValue_ pKey_ pType_ =
  Condition' {value = pValue_, key = pKey_, type' = pType_}

-- | Specifies the value for the key. Currently, this must be the ID of the organization.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValue :: Lens.Lens' Condition Lude.Text
cValue = Lens.lens (value :: Condition -> Lude.Text) (\s a -> s {value = a} :: Condition)
{-# DEPRECATED cValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKey :: Lens.Lens' Condition Lude.Text
cKey = Lens.lens (key :: Condition -> Lude.Text) (\s a -> s {key = a} :: Condition)
{-# DEPRECATED cKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Specifies the type of condition. Currently the only supported value is @StringEquals@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Condition Lude.Text
cType = Lens.lens (type' :: Condition -> Lude.Text) (\s a -> s {type' = a} :: Condition)
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON Condition where
  toJSON Condition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Value" Lude..= value),
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("Type" Lude..= type')
          ]
      )
