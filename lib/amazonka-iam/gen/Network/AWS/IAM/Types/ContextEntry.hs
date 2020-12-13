{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ContextEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ContextEntry
  ( ContextEntry (..),

    -- * Smart constructor
    mkContextEntry,

    -- * Lenses
    ceContextKeyValues,
    ceContextKeyName,
    ceContextKeyType,
  )
where

import Network.AWS.IAM.Types.ContextKeyTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a condition context key. It includes the name of the key and specifies the value (or values, if the context key supports multiple values) to use in the simulation. This information is used when evaluating the @Condition@ elements of the input policies.
--
-- This data type is used as an input parameter to 'SimulateCustomPolicy' and 'SimulatePrincipalPolicy' .
--
-- /See:/ 'mkContextEntry' smart constructor.
data ContextEntry = ContextEntry'
  { -- | The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
    contextKeyValues :: Lude.Maybe [Lude.Text],
    -- | The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
    contextKeyName :: Lude.Maybe Lude.Text,
    -- | The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
    contextKeyType :: Lude.Maybe ContextKeyTypeEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContextEntry' with the minimum fields required to make a request.
--
-- * 'contextKeyValues' - The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
-- * 'contextKeyName' - The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
-- * 'contextKeyType' - The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
mkContextEntry ::
  ContextEntry
mkContextEntry =
  ContextEntry'
    { contextKeyValues = Lude.Nothing,
      contextKeyName = Lude.Nothing,
      contextKeyType = Lude.Nothing
    }

-- | The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
--
-- /Note:/ Consider using 'contextKeyValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceContextKeyValues :: Lens.Lens' ContextEntry (Lude.Maybe [Lude.Text])
ceContextKeyValues = Lens.lens (contextKeyValues :: ContextEntry -> Lude.Maybe [Lude.Text]) (\s a -> s {contextKeyValues = a} :: ContextEntry)
{-# DEPRECATED ceContextKeyValues "Use generic-lens or generic-optics with 'contextKeyValues' instead." #-}

-- | The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
--
-- /Note:/ Consider using 'contextKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceContextKeyName :: Lens.Lens' ContextEntry (Lude.Maybe Lude.Text)
ceContextKeyName = Lens.lens (contextKeyName :: ContextEntry -> Lude.Maybe Lude.Text) (\s a -> s {contextKeyName = a} :: ContextEntry)
{-# DEPRECATED ceContextKeyName "Use generic-lens or generic-optics with 'contextKeyName' instead." #-}

-- | The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
--
-- /Note:/ Consider using 'contextKeyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceContextKeyType :: Lens.Lens' ContextEntry (Lude.Maybe ContextKeyTypeEnum)
ceContextKeyType = Lens.lens (contextKeyType :: ContextEntry -> Lude.Maybe ContextKeyTypeEnum) (\s a -> s {contextKeyType = a} :: ContextEntry)
{-# DEPRECATED ceContextKeyType "Use generic-lens or generic-optics with 'contextKeyType' instead." #-}

instance Lude.ToQuery ContextEntry where
  toQuery ContextEntry' {..} =
    Lude.mconcat
      [ "ContextKeyValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> contextKeyValues),
        "ContextKeyName" Lude.=: contextKeyName,
        "ContextKeyType" Lude.=: contextKeyType
      ]
