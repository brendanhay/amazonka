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
-- Module      : Amazonka.Lightsail.Types.Container
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Container where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.ContainerServiceProtocol
import qualified Amazonka.Prelude as Prelude

-- | Describes the settings of a container that will be launched, or that is
-- launched, to an Amazon Lightsail container service.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | The launch command for the container.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The environment variables of the container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the image used for the container.
    --
    -- Container images sourced from your Lightsail container service, that are
    -- registered and stored on your service, start with a colon (@:@). For
    -- example, if your container service name is @container-service-1@, the
    -- container image label is @mystaticsite@, and you want to use the third
    -- (@3@) version of the registered container image, then you should specify
    -- @:container-service-1.mystaticsite.3@. To use the latest version of a
    -- container image, specify @latest@ instead of a version number (for
    -- example, @:container-service-1.mystaticsite.latest@). Lightsail will
    -- automatically use the highest numbered version of the registered
    -- container image.
    --
    -- Container images sourced from a public registry like Docker Hub don\'t
    -- start with a colon. For example, @nginx:latest@ or @nginx@.
    image :: Prelude.Maybe Prelude.Text,
    -- | The open firewall ports of the container.
    ports :: Prelude.Maybe (Prelude.HashMap Prelude.Text ContainerServiceProtocol)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Container' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'container_command' - The launch command for the container.
--
-- 'environment', 'container_environment' - The environment variables of the container.
--
-- 'image', 'container_image' - The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are
-- registered and stored on your service, start with a colon (@:@). For
-- example, if your container service name is @container-service-1@, the
-- container image label is @mystaticsite@, and you want to use the third
-- (@3@) version of the registered container image, then you should specify
-- @:container-service-1.mystaticsite.3@. To use the latest version of a
-- container image, specify @latest@ instead of a version number (for
-- example, @:container-service-1.mystaticsite.latest@). Lightsail will
-- automatically use the highest numbered version of the registered
-- container image.
--
-- Container images sourced from a public registry like Docker Hub don\'t
-- start with a colon. For example, @nginx:latest@ or @nginx@.
--
-- 'ports', 'container_ports' - The open firewall ports of the container.
newContainer ::
  Container
newContainer =
  Container'
    { command = Prelude.Nothing,
      environment = Prelude.Nothing,
      image = Prelude.Nothing,
      ports = Prelude.Nothing
    }

-- | The launch command for the container.
container_command :: Lens.Lens' Container (Prelude.Maybe [Prelude.Text])
container_command = Lens.lens (\Container' {command} -> command) (\s@Container' {} a -> s {command = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | The environment variables of the container.
container_environment :: Lens.Lens' Container (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
container_environment = Lens.lens (\Container' {environment} -> environment) (\s@Container' {} a -> s {environment = a} :: Container) Prelude.. Lens.mapping Lens.coerced

-- | The name of the image used for the container.
--
-- Container images sourced from your Lightsail container service, that are
-- registered and stored on your service, start with a colon (@:@). For
-- example, if your container service name is @container-service-1@, the
-- container image label is @mystaticsite@, and you want to use the third
-- (@3@) version of the registered container image, then you should specify
-- @:container-service-1.mystaticsite.3@. To use the latest version of a
-- container image, specify @latest@ instead of a version number (for
-- example, @:container-service-1.mystaticsite.latest@). Lightsail will
-- automatically use the highest numbered version of the registered
-- container image.
--
-- Container images sourced from a public registry like Docker Hub don\'t
-- start with a colon. For example, @nginx:latest@ or @nginx@.
container_image :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_image = Lens.lens (\Container' {image} -> image) (\s@Container' {} a -> s {image = a} :: Container)

-- | The open firewall ports of the container.
container_ports :: Lens.Lens' Container (Prelude.Maybe (Prelude.HashMap Prelude.Text ContainerServiceProtocol))
container_ports = Lens.lens (\Container' {ports} -> ports) (\s@Container' {} a -> s {ports = a} :: Container) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Container where
  parseJSON =
    Data.withObject
      "Container"
      ( \x ->
          Container'
            Prelude.<$> (x Data..:? "command" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "image")
            Prelude.<*> (x Data..:? "ports" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Container where
  hashWithSalt _salt Container' {..} =
    _salt
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` ports

instance Prelude.NFData Container where
  rnf Container' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf ports

instance Data.ToJSON Container where
  toJSON Container' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("command" Data..=) Prelude.<$> command,
            ("environment" Data..=) Prelude.<$> environment,
            ("image" Data..=) Prelude.<$> image,
            ("ports" Data..=) Prelude.<$> ports
          ]
      )
