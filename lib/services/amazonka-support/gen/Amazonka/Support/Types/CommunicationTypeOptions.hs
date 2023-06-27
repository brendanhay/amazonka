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
-- Module      : Amazonka.Support.Types.CommunicationTypeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.CommunicationTypeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Support.Types.DateInterval
import Amazonka.Support.Types.SupportedHour

-- | A JSON-formatted object that contains the CommunicationTypeOptions for
-- creating a case for a certain communication channel. It is contained in
-- the response from a DescribeCreateCaseOptions request.
-- __CommunicationTypeOptions__ contains the following fields:
--
-- -   __datesWithoutSupport__ - A JSON-formatted list containing date and
--     time ranges for periods without support in UTC time. Date and time
--     format is RFC 3339 : \'yyyy-MM-dd\'T\'HH:mm:ss.SSSZZ\'.
--
-- -   __supportedHours__ - A JSON-formatted list containing time ranges
--     when support are available. Time format is RFC 3339 :
--     \'HH:mm:ss.SSS\'.
--
-- -   __type__ - A string value indicating the communication type that the
--     aforementioned rules apply to. At the moment the type value can
--     assume one of 3 values at the moment @chat@, @web@ and @call@.
--
-- /See:/ 'newCommunicationTypeOptions' smart constructor.
data CommunicationTypeOptions = CommunicationTypeOptions'
  { -- | A JSON-formatted list containing date and time ranges for periods
    -- without support
    datesWithoutSupport :: Prelude.Maybe [DateInterval],
    -- | A JSON-formatted list containing time ranges when support is available.
    supportedHours :: Prelude.Maybe [SupportedHour],
    -- | A string value indicating the communication type. At the moment the type
    -- value can assume one of 3 values at the moment chat, web and call.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommunicationTypeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datesWithoutSupport', 'communicationTypeOptions_datesWithoutSupport' - A JSON-formatted list containing date and time ranges for periods
-- without support
--
-- 'supportedHours', 'communicationTypeOptions_supportedHours' - A JSON-formatted list containing time ranges when support is available.
--
-- 'type'', 'communicationTypeOptions_type' - A string value indicating the communication type. At the moment the type
-- value can assume one of 3 values at the moment chat, web and call.
newCommunicationTypeOptions ::
  CommunicationTypeOptions
newCommunicationTypeOptions =
  CommunicationTypeOptions'
    { datesWithoutSupport =
        Prelude.Nothing,
      supportedHours = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A JSON-formatted list containing date and time ranges for periods
-- without support
communicationTypeOptions_datesWithoutSupport :: Lens.Lens' CommunicationTypeOptions (Prelude.Maybe [DateInterval])
communicationTypeOptions_datesWithoutSupport = Lens.lens (\CommunicationTypeOptions' {datesWithoutSupport} -> datesWithoutSupport) (\s@CommunicationTypeOptions' {} a -> s {datesWithoutSupport = a} :: CommunicationTypeOptions) Prelude.. Lens.mapping Lens.coerced

-- | A JSON-formatted list containing time ranges when support is available.
communicationTypeOptions_supportedHours :: Lens.Lens' CommunicationTypeOptions (Prelude.Maybe [SupportedHour])
communicationTypeOptions_supportedHours = Lens.lens (\CommunicationTypeOptions' {supportedHours} -> supportedHours) (\s@CommunicationTypeOptions' {} a -> s {supportedHours = a} :: CommunicationTypeOptions) Prelude.. Lens.mapping Lens.coerced

-- | A string value indicating the communication type. At the moment the type
-- value can assume one of 3 values at the moment chat, web and call.
communicationTypeOptions_type :: Lens.Lens' CommunicationTypeOptions (Prelude.Maybe Prelude.Text)
communicationTypeOptions_type = Lens.lens (\CommunicationTypeOptions' {type'} -> type') (\s@CommunicationTypeOptions' {} a -> s {type' = a} :: CommunicationTypeOptions)

instance Data.FromJSON CommunicationTypeOptions where
  parseJSON =
    Data.withObject
      "CommunicationTypeOptions"
      ( \x ->
          CommunicationTypeOptions'
            Prelude.<$> ( x
                            Data..:? "datesWithoutSupport"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "supportedHours" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable CommunicationTypeOptions where
  hashWithSalt _salt CommunicationTypeOptions' {..} =
    _salt
      `Prelude.hashWithSalt` datesWithoutSupport
      `Prelude.hashWithSalt` supportedHours
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CommunicationTypeOptions where
  rnf CommunicationTypeOptions' {..} =
    Prelude.rnf datesWithoutSupport
      `Prelude.seq` Prelude.rnf supportedHours
      `Prelude.seq` Prelude.rnf type'
