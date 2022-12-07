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
-- Module      : Amazonka.Kendra.Types.ExperienceEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ExperienceEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.EndpointType
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for the endpoint for your Amazon
-- Kendra experience.
--
-- /See:/ 'newExperienceEndpoint' smart constructor.
data ExperienceEndpoint = ExperienceEndpoint'
  { -- | The type of endpoint for your Amazon Kendra experience. The type
    -- currently available is @HOME@, which is a unique and fully hosted URL to
    -- the home page of your Amazon Kendra experience.
    endpointType :: Prelude.Maybe EndpointType,
    -- | The endpoint of your Amazon Kendra experience.
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperienceEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointType', 'experienceEndpoint_endpointType' - The type of endpoint for your Amazon Kendra experience. The type
-- currently available is @HOME@, which is a unique and fully hosted URL to
-- the home page of your Amazon Kendra experience.
--
-- 'endpoint', 'experienceEndpoint_endpoint' - The endpoint of your Amazon Kendra experience.
newExperienceEndpoint ::
  ExperienceEndpoint
newExperienceEndpoint =
  ExperienceEndpoint'
    { endpointType = Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The type of endpoint for your Amazon Kendra experience. The type
-- currently available is @HOME@, which is a unique and fully hosted URL to
-- the home page of your Amazon Kendra experience.
experienceEndpoint_endpointType :: Lens.Lens' ExperienceEndpoint (Prelude.Maybe EndpointType)
experienceEndpoint_endpointType = Lens.lens (\ExperienceEndpoint' {endpointType} -> endpointType) (\s@ExperienceEndpoint' {} a -> s {endpointType = a} :: ExperienceEndpoint)

-- | The endpoint of your Amazon Kendra experience.
experienceEndpoint_endpoint :: Lens.Lens' ExperienceEndpoint (Prelude.Maybe Prelude.Text)
experienceEndpoint_endpoint = Lens.lens (\ExperienceEndpoint' {endpoint} -> endpoint) (\s@ExperienceEndpoint' {} a -> s {endpoint = a} :: ExperienceEndpoint)

instance Data.FromJSON ExperienceEndpoint where
  parseJSON =
    Data.withObject
      "ExperienceEndpoint"
      ( \x ->
          ExperienceEndpoint'
            Prelude.<$> (x Data..:? "EndpointType")
            Prelude.<*> (x Data..:? "Endpoint")
      )

instance Prelude.Hashable ExperienceEndpoint where
  hashWithSalt _salt ExperienceEndpoint' {..} =
    _salt `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData ExperienceEndpoint where
  rnf ExperienceEndpoint' {..} =
    Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf endpoint
