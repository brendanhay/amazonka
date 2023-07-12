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
-- Module      : Amazonka.APIGateway.Types.Deployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Deployment where

import Amazonka.APIGateway.Types.MethodSnapshot
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An immutable representation of a RestApi resource that can be called by
-- users using Stages. A deployment must be associated with a Stage for it
-- to be callable over the Internet.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | A summary of the RestApi at the date and time that the deployment
    -- resource was created.
    apiSummary :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text MethodSnapshot)),
    -- | The date and time that the deployment resource was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The description for the deployment resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the deployment resource.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiSummary', 'deployment_apiSummary' - A summary of the RestApi at the date and time that the deployment
-- resource was created.
--
-- 'createdDate', 'deployment_createdDate' - The date and time that the deployment resource was created.
--
-- 'description', 'deployment_description' - The description for the deployment resource.
--
-- 'id', 'deployment_id' - The identifier for the deployment resource.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { apiSummary = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | A summary of the RestApi at the date and time that the deployment
-- resource was created.
deployment_apiSummary :: Lens.Lens' Deployment (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text MethodSnapshot)))
deployment_apiSummary = Lens.lens (\Deployment' {apiSummary} -> apiSummary) (\s@Deployment' {} a -> s {apiSummary = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the deployment resource was created.
deployment_createdDate :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_createdDate = Lens.lens (\Deployment' {createdDate} -> createdDate) (\s@Deployment' {} a -> s {createdDate = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | The description for the deployment resource.
deployment_description :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_description = Lens.lens (\Deployment' {description} -> description) (\s@Deployment' {} a -> s {description = a} :: Deployment)

-- | The identifier for the deployment resource.
deployment_id :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_id = Lens.lens (\Deployment' {id} -> id) (\s@Deployment' {} a -> s {id = a} :: Deployment)

instance Data.FromJSON Deployment where
  parseJSON =
    Data.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Data..:? "apiSummary" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt
      `Prelude.hashWithSalt` apiSummary
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf apiSummary
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
