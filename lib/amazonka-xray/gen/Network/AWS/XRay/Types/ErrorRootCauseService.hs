-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCauseService
  ( ErrorRootCauseService (..),

    -- * Smart constructor
    mkErrorRootCauseService,

    -- * Lenses
    ercsEntityPath,
    ercsAccountId,
    ercsNames,
    ercsName,
    ercsInferred,
    ercsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.ErrorRootCauseEntity

-- | A collection of fields identifying the services in a trace summary error.
--
-- /See:/ 'mkErrorRootCauseService' smart constructor.
data ErrorRootCauseService = ErrorRootCauseService'
  { entityPath ::
      Lude.Maybe [ErrorRootCauseEntity],
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

-- | Creates a value of 'ErrorRootCauseService' with the minimum fields required to make a request.
--
-- * 'accountId' - The account ID associated to the service.
-- * 'entityPath' - The path of root cause entities found on the service.
-- * 'inferred' - A Boolean value indicating if the service is inferred from the trace.
-- * 'name' - The service name.
-- * 'names' - A collection of associated service names.
-- * 'type'' - The type associated to the service.
mkErrorRootCauseService ::
  ErrorRootCauseService
mkErrorRootCauseService =
  ErrorRootCauseService'
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
ercsEntityPath :: Lens.Lens' ErrorRootCauseService (Lude.Maybe [ErrorRootCauseEntity])
ercsEntityPath = Lens.lens (entityPath :: ErrorRootCauseService -> Lude.Maybe [ErrorRootCauseEntity]) (\s a -> s {entityPath = a} :: ErrorRootCauseService)
{-# DEPRECATED ercsEntityPath "Use generic-lens or generic-optics with 'entityPath' instead." #-}

-- | The account ID associated to the service.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsAccountId :: Lens.Lens' ErrorRootCauseService (Lude.Maybe Lude.Text)
ercsAccountId = Lens.lens (accountId :: ErrorRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ErrorRootCauseService)
{-# DEPRECATED ercsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A collection of associated service names.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsNames :: Lens.Lens' ErrorRootCauseService (Lude.Maybe [Lude.Text])
ercsNames = Lens.lens (names :: ErrorRootCauseService -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: ErrorRootCauseService)
{-# DEPRECATED ercsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The service name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsName :: Lens.Lens' ErrorRootCauseService (Lude.Maybe Lude.Text)
ercsName = Lens.lens (name :: ErrorRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ErrorRootCauseService)
{-# DEPRECATED ercsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A Boolean value indicating if the service is inferred from the trace.
--
-- /Note:/ Consider using 'inferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsInferred :: Lens.Lens' ErrorRootCauseService (Lude.Maybe Lude.Bool)
ercsInferred = Lens.lens (inferred :: ErrorRootCauseService -> Lude.Maybe Lude.Bool) (\s a -> s {inferred = a} :: ErrorRootCauseService)
{-# DEPRECATED ercsInferred "Use generic-lens or generic-optics with 'inferred' instead." #-}

-- | The type associated to the service.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsType :: Lens.Lens' ErrorRootCauseService (Lude.Maybe Lude.Text)
ercsType = Lens.lens (type' :: ErrorRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ErrorRootCauseService)
{-# DEPRECATED ercsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ErrorRootCauseService where
  parseJSON =
    Lude.withObject
      "ErrorRootCauseService"
      ( \x ->
          ErrorRootCauseService'
            Lude.<$> (x Lude..:? "EntityPath" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "Names" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Inferred")
            Lude.<*> (x Lude..:? "Type")
      )
