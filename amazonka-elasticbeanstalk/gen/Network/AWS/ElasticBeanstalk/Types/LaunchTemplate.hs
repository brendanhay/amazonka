{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LaunchTemplate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon EC2 launch template.
--
-- /See:/ 'newLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { -- | The ID of the launch template.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'launchTemplate_id' - The ID of the launch template.
newLaunchTemplate ::
  LaunchTemplate
newLaunchTemplate =
  LaunchTemplate' {id = Prelude.Nothing}

-- | The ID of the launch template.
launchTemplate_id :: Lens.Lens' LaunchTemplate (Prelude.Maybe Prelude.Text)
launchTemplate_id = Lens.lens (\LaunchTemplate' {id} -> id) (\s@LaunchTemplate' {} a -> s {id = a} :: LaunchTemplate)

instance Prelude.FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate' Prelude.<$> (x Prelude..@? "Id")

instance Prelude.Hashable LaunchTemplate

instance Prelude.NFData LaunchTemplate
