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
-- Module      : Network.AWS.KinesisAnalytics.Types.InputConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputConfiguration where

import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When you start your application, you provide this configuration, which
-- identifies the input source and the point in the input source at which
-- you want the application to start processing records.
--
-- /See:/ 'newInputConfiguration' smart constructor.
data InputConfiguration = InputConfiguration'
  { -- | Input source ID. You can get this ID by calling the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation.
    id :: Prelude.Text,
    -- | Point at which you want the application to start processing records from
    -- the streaming source.
    inputStartingPositionConfiguration :: InputStartingPositionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'inputConfiguration_id' - Input source ID. You can get this ID by calling the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
--
-- 'inputStartingPositionConfiguration', 'inputConfiguration_inputStartingPositionConfiguration' - Point at which you want the application to start processing records from
-- the streaming source.
newInputConfiguration ::
  -- | 'id'
  Prelude.Text ->
  -- | 'inputStartingPositionConfiguration'
  InputStartingPositionConfiguration ->
  InputConfiguration
newInputConfiguration
  pId_
  pInputStartingPositionConfiguration_ =
    InputConfiguration'
      { id = pId_,
        inputStartingPositionConfiguration =
          pInputStartingPositionConfiguration_
      }

-- | Input source ID. You can get this ID by calling the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
inputConfiguration_id :: Lens.Lens' InputConfiguration Prelude.Text
inputConfiguration_id = Lens.lens (\InputConfiguration' {id} -> id) (\s@InputConfiguration' {} a -> s {id = a} :: InputConfiguration)

-- | Point at which you want the application to start processing records from
-- the streaming source.
inputConfiguration_inputStartingPositionConfiguration :: Lens.Lens' InputConfiguration InputStartingPositionConfiguration
inputConfiguration_inputStartingPositionConfiguration = Lens.lens (\InputConfiguration' {inputStartingPositionConfiguration} -> inputStartingPositionConfiguration) (\s@InputConfiguration' {} a -> s {inputStartingPositionConfiguration = a} :: InputConfiguration)

instance Prelude.Hashable InputConfiguration

instance Prelude.NFData InputConfiguration

instance Prelude.ToJSON InputConfiguration where
  toJSON InputConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Prelude..= id),
            Prelude.Just
              ( "InputStartingPositionConfiguration"
                  Prelude..= inputStartingPositionConfiguration
              )
          ]
      )
