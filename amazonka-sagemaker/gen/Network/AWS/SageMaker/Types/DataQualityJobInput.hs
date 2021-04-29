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
-- Module      : Network.AWS.SageMaker.Types.DataQualityJobInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataQualityJobInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.EndpointInput

-- | The input for the data quality monitoring job. Currently endpoints are
-- supported for input.
--
-- /See:/ 'newDataQualityJobInput' smart constructor.
data DataQualityJobInput = DataQualityJobInput'
  { endpointInput :: EndpointInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DataQualityJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointInput', 'dataQualityJobInput_endpointInput' - Undocumented member.
newDataQualityJobInput ::
  -- | 'endpointInput'
  EndpointInput ->
  DataQualityJobInput
newDataQualityJobInput pEndpointInput_ =
  DataQualityJobInput'
    { endpointInput =
        pEndpointInput_
    }

-- | Undocumented member.
dataQualityJobInput_endpointInput :: Lens.Lens' DataQualityJobInput EndpointInput
dataQualityJobInput_endpointInput = Lens.lens (\DataQualityJobInput' {endpointInput} -> endpointInput) (\s@DataQualityJobInput' {} a -> s {endpointInput = a} :: DataQualityJobInput)

instance Prelude.FromJSON DataQualityJobInput where
  parseJSON =
    Prelude.withObject
      "DataQualityJobInput"
      ( \x ->
          DataQualityJobInput'
            Prelude.<$> (x Prelude..: "EndpointInput")
      )

instance Prelude.Hashable DataQualityJobInput

instance Prelude.NFData DataQualityJobInput

instance Prelude.ToJSON DataQualityJobInput where
  toJSON DataQualityJobInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointInput" Prelude..= endpointInput)
          ]
      )
