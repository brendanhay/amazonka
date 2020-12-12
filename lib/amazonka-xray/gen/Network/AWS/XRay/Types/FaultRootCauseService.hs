{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCauseService
  ( FaultRootCauseService (..),

    -- * Smart constructor
    mkFaultRootCauseService,

    -- * Lenses
    frcsEntityPath,
    frcsAccountId,
    frcsNames,
    frcsName,
    frcsInferred,
    frcsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.FaultRootCauseEntity

-- | A collection of fields identifying the services in a trace summary fault.
--
-- /See:/ 'mkFaultRootCauseService' smart constructor.
data FaultRootCauseService = FaultRootCauseService'
  { entityPath ::
      Lude.Maybe [FaultRootCauseEntity],
    accountId :: Lude.Maybe Lude.Text,
    names :: Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    inferred :: Lude.Maybe Lude.Bool,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FaultRootCauseService' with the minimum fields required to make a request.
--
-- * 'accountId' - The account ID associated to the service.
-- * 'entityPath' - The path of root cause entities found on the service.
-- * 'inferred' - A Boolean value indicating if the service is inferred from the trace.
-- * 'name' - The service name.
-- * 'names' - A collection of associated service names.
-- * 'type'' - The type associated to the service.
mkFaultRootCauseService ::
  FaultRootCauseService
mkFaultRootCauseService =
  FaultRootCauseService'
    { entityPath = Lude.Nothing,
      accountId = Lude.Nothing,
      names = Lude.Nothing,
      name = Lude.Nothing,
      inferred = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The path of root cause entities found on the service.
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsEntityPath :: Lens.Lens' FaultRootCauseService (Lude.Maybe [FaultRootCauseEntity])
frcsEntityPath = Lens.lens (entityPath :: FaultRootCauseService -> Lude.Maybe [FaultRootCauseEntity]) (\s a -> s {entityPath = a} :: FaultRootCauseService)
{-# DEPRECATED frcsEntityPath "Use generic-lens or generic-optics with 'entityPath' instead." #-}

-- | The account ID associated to the service.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsAccountId :: Lens.Lens' FaultRootCauseService (Lude.Maybe Lude.Text)
frcsAccountId = Lens.lens (accountId :: FaultRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: FaultRootCauseService)
{-# DEPRECATED frcsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A collection of associated service names.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsNames :: Lens.Lens' FaultRootCauseService (Lude.Maybe [Lude.Text])
frcsNames = Lens.lens (names :: FaultRootCauseService -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: FaultRootCauseService)
{-# DEPRECATED frcsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The service name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsName :: Lens.Lens' FaultRootCauseService (Lude.Maybe Lude.Text)
frcsName = Lens.lens (name :: FaultRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FaultRootCauseService)
{-# DEPRECATED frcsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A Boolean value indicating if the service is inferred from the trace.
--
-- /Note:/ Consider using 'inferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsInferred :: Lens.Lens' FaultRootCauseService (Lude.Maybe Lude.Bool)
frcsInferred = Lens.lens (inferred :: FaultRootCauseService -> Lude.Maybe Lude.Bool) (\s a -> s {inferred = a} :: FaultRootCauseService)
{-# DEPRECATED frcsInferred "Use generic-lens or generic-optics with 'inferred' instead." #-}

-- | The type associated to the service.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsType :: Lens.Lens' FaultRootCauseService (Lude.Maybe Lude.Text)
frcsType = Lens.lens (type' :: FaultRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: FaultRootCauseService)
{-# DEPRECATED frcsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON FaultRootCauseService where
  parseJSON =
    Lude.withObject
      "FaultRootCauseService"
      ( \x ->
          FaultRootCauseService'
            Lude.<$> (x Lude..:? "EntityPath" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "Names" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Inferred")
            Lude.<*> (x Lude..:? "Type")
      )
