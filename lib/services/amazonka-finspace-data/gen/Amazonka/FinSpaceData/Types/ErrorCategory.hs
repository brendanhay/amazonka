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
-- Module      : Amazonka.FinSpaceData.Types.ErrorCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ErrorCategory
  ( ErrorCategory
      ( ..,
        ErrorCategory_A_user_recoverable_error_has_occurred,
        ErrorCategory_An_internal_error_has_occurred,
        ErrorCategory_Cancelled,
        ErrorCategory_Missing_required_permission_to_perform_this_request,
        ErrorCategory_One_or_more_inputs_to_this_request_were_not_found,
        ErrorCategory_Service_limits_have_been_exceeded,
        ErrorCategory_The_inputs_to_this_request_are_invalid,
        ErrorCategory_The_system_temporarily_lacks_sufficient_resources_to_process_the_request
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ErrorCategory = ErrorCategory'
  { fromErrorCategory ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ErrorCategory_A_user_recoverable_error_has_occurred :: ErrorCategory
pattern ErrorCategory_A_user_recoverable_error_has_occurred = ErrorCategory' "A_user_recoverable_error_has_occurred"

pattern ErrorCategory_An_internal_error_has_occurred :: ErrorCategory
pattern ErrorCategory_An_internal_error_has_occurred = ErrorCategory' "An_internal_error_has_occurred"

pattern ErrorCategory_Cancelled :: ErrorCategory
pattern ErrorCategory_Cancelled = ErrorCategory' "Cancelled"

pattern ErrorCategory_Missing_required_permission_to_perform_this_request :: ErrorCategory
pattern ErrorCategory_Missing_required_permission_to_perform_this_request = ErrorCategory' "Missing_required_permission_to_perform_this_request"

pattern ErrorCategory_One_or_more_inputs_to_this_request_were_not_found :: ErrorCategory
pattern ErrorCategory_One_or_more_inputs_to_this_request_were_not_found = ErrorCategory' "One_or_more_inputs_to_this_request_were_not_found"

pattern ErrorCategory_Service_limits_have_been_exceeded :: ErrorCategory
pattern ErrorCategory_Service_limits_have_been_exceeded = ErrorCategory' "Service_limits_have_been_exceeded"

pattern ErrorCategory_The_inputs_to_this_request_are_invalid :: ErrorCategory
pattern ErrorCategory_The_inputs_to_this_request_are_invalid = ErrorCategory' "The_inputs_to_this_request_are_invalid"

pattern ErrorCategory_The_system_temporarily_lacks_sufficient_resources_to_process_the_request :: ErrorCategory
pattern ErrorCategory_The_system_temporarily_lacks_sufficient_resources_to_process_the_request = ErrorCategory' "The_system_temporarily_lacks_sufficient_resources_to_process_the_request"

{-# COMPLETE
  ErrorCategory_A_user_recoverable_error_has_occurred,
  ErrorCategory_An_internal_error_has_occurred,
  ErrorCategory_Cancelled,
  ErrorCategory_Missing_required_permission_to_perform_this_request,
  ErrorCategory_One_or_more_inputs_to_this_request_were_not_found,
  ErrorCategory_Service_limits_have_been_exceeded,
  ErrorCategory_The_inputs_to_this_request_are_invalid,
  ErrorCategory_The_system_temporarily_lacks_sufficient_resources_to_process_the_request,
  ErrorCategory'
  #-}
