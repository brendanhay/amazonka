{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpace.Types.ErrorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.ErrorDetails
  ( ErrorDetails
      ( ..,
        ErrorDetails_A_user_recoverable_error_has_occurred,
        ErrorDetails_An_internal_error_has_occurred_,
        ErrorDetails_Cancelled,
        ErrorDetails_Missing_required_permission_to_perform_this_request_,
        ErrorDetails_One_or_more_inputs_to_this_request_were_not_found_,
        ErrorDetails_Service_limits_have_been_exceeded_,
        ErrorDetails_The_inputs_to_this_request_are_invalid_,
        ErrorDetails_The_system_temporarily_lacks_sufficient_resources_to_process_the_request_
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ErrorDetails = ErrorDetails'
  { fromErrorDetails ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ErrorDetails_A_user_recoverable_error_has_occurred :: ErrorDetails
pattern ErrorDetails_A_user_recoverable_error_has_occurred = ErrorDetails' "A user recoverable error has occurred"

pattern ErrorDetails_An_internal_error_has_occurred_ :: ErrorDetails
pattern ErrorDetails_An_internal_error_has_occurred_ = ErrorDetails' "An internal error has occurred."

pattern ErrorDetails_Cancelled :: ErrorDetails
pattern ErrorDetails_Cancelled = ErrorDetails' "Cancelled"

pattern ErrorDetails_Missing_required_permission_to_perform_this_request_ :: ErrorDetails
pattern ErrorDetails_Missing_required_permission_to_perform_this_request_ = ErrorDetails' "Missing required permission to perform this request."

pattern ErrorDetails_One_or_more_inputs_to_this_request_were_not_found_ :: ErrorDetails
pattern ErrorDetails_One_or_more_inputs_to_this_request_were_not_found_ = ErrorDetails' "One or more inputs to this request were not found."

pattern ErrorDetails_Service_limits_have_been_exceeded_ :: ErrorDetails
pattern ErrorDetails_Service_limits_have_been_exceeded_ = ErrorDetails' "Service limits have been exceeded."

pattern ErrorDetails_The_inputs_to_this_request_are_invalid_ :: ErrorDetails
pattern ErrorDetails_The_inputs_to_this_request_are_invalid_ = ErrorDetails' "The inputs to this request are invalid."

pattern ErrorDetails_The_system_temporarily_lacks_sufficient_resources_to_process_the_request_ :: ErrorDetails
pattern ErrorDetails_The_system_temporarily_lacks_sufficient_resources_to_process_the_request_ = ErrorDetails' "The system temporarily lacks sufficient resources to process the request."

{-# COMPLETE
  ErrorDetails_A_user_recoverable_error_has_occurred,
  ErrorDetails_An_internal_error_has_occurred_,
  ErrorDetails_Cancelled,
  ErrorDetails_Missing_required_permission_to_perform_this_request_,
  ErrorDetails_One_or_more_inputs_to_this_request_were_not_found_,
  ErrorDetails_Service_limits_have_been_exceeded_,
  ErrorDetails_The_inputs_to_this_request_are_invalid_,
  ErrorDetails_The_system_temporarily_lacks_sufficient_resources_to_process_the_request_,
  ErrorDetails'
  #-}
