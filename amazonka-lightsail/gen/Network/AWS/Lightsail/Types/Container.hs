{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Container
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Container where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ContainerServiceProtocol

-- | Describes the settings of a container that will be launched, or that is
-- launched, to an Amazon Lightsail container service.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | The environment variables of the container.
    environment :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The open firewall ports of the container.
    ports :: Core.Maybe (Core.HashMap Core.Text ContainerServiceProtocol),
    -- | The name of the image used for the container.
    --
    -- Container images sourced from your Lightsail container service, that are
    -- registered and stored on your service, start with a colon (@:@). For
    -- example, @:container-service-1.mystaticwebsite.1@. Container images
    -- sourced from a public registry like Docker Hub don\'t start with a
    -- colon. For example, @nginx:latest@ or @nginx@.
    image :: Core.Maybe Core.Text,
    -- | The launch command for the container.
    command :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Container' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'container_environment' - The environment variables of the container.
--
-- 'ports', 'container_ports' - The open firewall ports of the container.
--
-- 'image', 'container_image' - The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are
-- registered and stored on your service, start with a colon (@:@). For
-- example, @:container-service-1.mystaticwebsite.1@. Container images
-- sourced from a public registry like Docker Hub don\'t start with a
-- colon. For example, @nginx:latest@ or @nginx@.
--
-- 'command', 'container_command' - The launch command for the container.
newContainer ::
  Container
newContainer =
  Container'
    { environment = Core.Nothing,
      ports = Core.Nothing,
      image = Core.Nothing,
      command = Core.Nothing
    }

-- | The environment variables of the container.
container_environment :: Lens.Lens' Container (Core.Maybe (Core.HashMap Core.Text Core.Text))
container_environment = Lens.lens (\Container' {environment} -> environment) (\s@Container' {} a -> s {environment = a} :: Container) Core.. Lens.mapping Lens._Coerce

-- | The open firewall ports of the container.
container_ports :: Lens.Lens' Container (Core.Maybe (Core.HashMap Core.Text ContainerServiceProtocol))
container_ports = Lens.lens (\Container' {ports} -> ports) (\s@Container' {} a -> s {ports = a} :: Container) Core.. Lens.mapping Lens._Coerce

-- | The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are
-- registered and stored on your service, start with a colon (@:@). For
-- example, @:container-service-1.mystaticwebsite.1@. Container images
-- sourced from a public registry like Docker Hub don\'t start with a
-- colon. For example, @nginx:latest@ or @nginx@.
container_image :: Lens.Lens' Container (Core.Maybe Core.Text)
container_image = Lens.lens (\Container' {image} -> image) (\s@Container' {} a -> s {image = a} :: Container)

-- | The launch command for the container.
container_command :: Lens.Lens' Container (Core.Maybe [Core.Text])
container_command = Lens.lens (\Container' {command} -> command) (\s@Container' {} a -> s {command = a} :: Container) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Container where
  parseJSON =
    Core.withObject
      "Container"
      ( \x ->
          Container'
            Core.<$> (x Core..:? "environment" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ports" Core..!= Core.mempty)
            Core.<*> (x Core..:? "image")
            Core.<*> (x Core..:? "command" Core..!= Core.mempty)
      )

instance Core.Hashable Container

instance Core.NFData Container

instance Core.ToJSON Container where
  toJSON Container' {..} =
    Core.object
      ( Core.catMaybes
          [ ("environment" Core..=) Core.<$> environment,
            ("ports" Core..=) Core.<$> ports,
            ("image" Core..=) Core.<$> image,
            ("command" Core..=) Core.<$> command
          ]
      )
