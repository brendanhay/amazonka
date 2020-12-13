{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCauseService
  ( ResponseTimeRootCauseService (..),

    -- * Smart constructor
    mkResponseTimeRootCauseService,

    -- * Lenses
    rtrcsEntityPath,
    rtrcsAccountId,
    rtrcsNames,
    rtrcsName,
    rtrcsInferred,
    rtrcsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.ResponseTimeRootCauseEntity

-- | A collection of fields identifying the service in a response time warning.
--
-- /See:/ 'mkResponseTimeRootCauseService' smart constructor.
data ResponseTimeRootCauseService = ResponseTimeRootCauseService'
  { -- | The path of root cause entities found on the service.
    entityPath :: Lude.Maybe [ResponseTimeRootCauseEntity],
    -- | The account ID associated to the service.
    accountId :: Lude.Maybe Lude.Text,
    -- | A collection of associated service names.
    names :: Lude.Maybe [Lude.Text],
    -- | The service name.
    name :: Lude.Maybe Lude.Text,
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Lude.Maybe Lude.Bool,
    -- | The type associated to the service.
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResponseTimeRootCauseService' with the minimum fields required to make a request.
--
-- * 'entityPath' - The path of root cause entities found on the service.
-- * 'accountId' - The account ID associated to the service.
-- * 'names' - A collection of associated service names.
-- * 'name' - The service name.
-- * 'inferred' - A Boolean value indicating if the service is inferred from the trace.
-- * 'type'' - The type associated to the service.
mkResponseTimeRootCauseService ::
  ResponseTimeRootCauseService
mkResponseTimeRootCauseService =
  ResponseTimeRootCauseService'
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
rtrcsEntityPath :: Lens.Lens' ResponseTimeRootCauseService (Lude.Maybe [ResponseTimeRootCauseEntity])
rtrcsEntityPath = Lens.lens (entityPath :: ResponseTimeRootCauseService -> Lude.Maybe [ResponseTimeRootCauseEntity]) (\s a -> s {entityPath = a} :: ResponseTimeRootCauseService)
{-# DEPRECATED rtrcsEntityPath "Use generic-lens or generic-optics with 'entityPath' instead." #-}

-- | The account ID associated to the service.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsAccountId :: Lens.Lens' ResponseTimeRootCauseService (Lude.Maybe Lude.Text)
rtrcsAccountId = Lens.lens (accountId :: ResponseTimeRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ResponseTimeRootCauseService)
{-# DEPRECATED rtrcsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A collection of associated service names.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsNames :: Lens.Lens' ResponseTimeRootCauseService (Lude.Maybe [Lude.Text])
rtrcsNames = Lens.lens (names :: ResponseTimeRootCauseService -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: ResponseTimeRootCauseService)
{-# DEPRECATED rtrcsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The service name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsName :: Lens.Lens' ResponseTimeRootCauseService (Lude.Maybe Lude.Text)
rtrcsName = Lens.lens (name :: ResponseTimeRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResponseTimeRootCauseService)
{-# DEPRECATED rtrcsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A Boolean value indicating if the service is inferred from the trace.
--
-- /Note:/ Consider using 'inferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsInferred :: Lens.Lens' ResponseTimeRootCauseService (Lude.Maybe Lude.Bool)
rtrcsInferred = Lens.lens (inferred :: ResponseTimeRootCauseService -> Lude.Maybe Lude.Bool) (\s a -> s {inferred = a} :: ResponseTimeRootCauseService)
{-# DEPRECATED rtrcsInferred "Use generic-lens or generic-optics with 'inferred' instead." #-}

-- | The type associated to the service.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsType :: Lens.Lens' ResponseTimeRootCauseService (Lude.Maybe Lude.Text)
rtrcsType = Lens.lens (type' :: ResponseTimeRootCauseService -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ResponseTimeRootCauseService)
{-# DEPRECATED rtrcsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ResponseTimeRootCauseService where
  parseJSON =
    Lude.withObject
      "ResponseTimeRootCauseService"
      ( \x ->
          ResponseTimeRootCauseService'
            Lude.<$> (x Lude..:? "EntityPath" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "Names" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Inferred")
            Lude.<*> (x Lude..:? "Type")
      )
