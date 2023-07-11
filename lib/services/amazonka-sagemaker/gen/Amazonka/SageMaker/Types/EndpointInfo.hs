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
-- Module      : Amazonka.SageMaker.Types.EndpointInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EndpointInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a customer endpoint that was compared in an Inference
-- Recommender job.
--
-- /See:/ 'newEndpointInfo' smart constructor.
data EndpointInfo = EndpointInfo'
  { -- | The name of a customer\'s endpoint.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'endpointInfo_endpointName' - The name of a customer\'s endpoint.
newEndpointInfo ::
  -- | 'endpointName'
  Prelude.Text ->
  EndpointInfo
newEndpointInfo pEndpointName_ =
  EndpointInfo' {endpointName = pEndpointName_}

-- | The name of a customer\'s endpoint.
endpointInfo_endpointName :: Lens.Lens' EndpointInfo Prelude.Text
endpointInfo_endpointName = Lens.lens (\EndpointInfo' {endpointName} -> endpointName) (\s@EndpointInfo' {} a -> s {endpointName = a} :: EndpointInfo)

instance Data.FromJSON EndpointInfo where
  parseJSON =
    Data.withObject
      "EndpointInfo"
      ( \x ->
          EndpointInfo' Prelude.<$> (x Data..: "EndpointName")
      )

instance Prelude.Hashable EndpointInfo where
  hashWithSalt _salt EndpointInfo' {..} =
    _salt `Prelude.hashWithSalt` endpointName

instance Prelude.NFData EndpointInfo where
  rnf EndpointInfo' {..} = Prelude.rnf endpointName

instance Data.ToJSON EndpointInfo where
  toJSON EndpointInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("EndpointName" Data..= endpointName)]
      )
