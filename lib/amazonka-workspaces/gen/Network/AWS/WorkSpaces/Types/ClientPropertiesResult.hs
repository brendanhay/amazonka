{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ClientPropertiesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ClientPropertiesResult
  ( ClientPropertiesResult (..),

    -- * Smart constructor
    mkClientPropertiesResult,

    -- * Lenses
    cprResourceId,
    cprClientProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.ClientProperties

-- | Information about the Amazon WorkSpaces client.
--
-- /See:/ 'mkClientPropertiesResult' smart constructor.
data ClientPropertiesResult = ClientPropertiesResult'
  { -- | The resource identifier, in the form of a directory ID.
    resourceId :: Lude.Maybe Lude.Text,
    -- | Information about the Amazon WorkSpaces client.
    clientProperties :: Lude.Maybe ClientProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientPropertiesResult' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource identifier, in the form of a directory ID.
-- * 'clientProperties' - Information about the Amazon WorkSpaces client.
mkClientPropertiesResult ::
  ClientPropertiesResult
mkClientPropertiesResult =
  ClientPropertiesResult'
    { resourceId = Lude.Nothing,
      clientProperties = Lude.Nothing
    }

-- | The resource identifier, in the form of a directory ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprResourceId :: Lens.Lens' ClientPropertiesResult (Lude.Maybe Lude.Text)
cprResourceId = Lens.lens (resourceId :: ClientPropertiesResult -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ClientPropertiesResult)
{-# DEPRECATED cprResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Information about the Amazon WorkSpaces client.
--
-- /Note:/ Consider using 'clientProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprClientProperties :: Lens.Lens' ClientPropertiesResult (Lude.Maybe ClientProperties)
cprClientProperties = Lens.lens (clientProperties :: ClientPropertiesResult -> Lude.Maybe ClientProperties) (\s a -> s {clientProperties = a} :: ClientPropertiesResult)
{-# DEPRECATED cprClientProperties "Use generic-lens or generic-optics with 'clientProperties' instead." #-}

instance Lude.FromJSON ClientPropertiesResult where
  parseJSON =
    Lude.withObject
      "ClientPropertiesResult"
      ( \x ->
          ClientPropertiesResult'
            Lude.<$> (x Lude..:? "ResourceId") Lude.<*> (x Lude..:? "ClientProperties")
      )
