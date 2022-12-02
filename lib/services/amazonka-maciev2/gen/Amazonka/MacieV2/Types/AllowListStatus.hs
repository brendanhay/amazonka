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
-- Module      : Amazonka.MacieV2.Types.AllowListStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AllowListStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.AllowListStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the current status of an allow list, which
-- indicates whether Amazon Macie can access and use the list\'s criteria.
--
-- /See:/ 'newAllowListStatus' smart constructor.
data AllowListStatus = AllowListStatus'
  { -- | A brief description of the status of the allow list. Amazon Macie uses
    -- this value to provide additional information about an error that
    -- occurred when Macie tried to access and use the list\'s criteria.
    description :: Prelude.Maybe Prelude.Text,
    -- | The current status of the allow list. If the list\'s criteria specify a
    -- regular expression (regex), this value is typically OK. Amazon Macie can
    -- compile the expression.
    --
    -- If the list\'s criteria specify an S3 object, possible values are:
    --
    -- -   OK - Macie can retrieve and parse the contents of the object.
    --
    -- -   S3_OBJECT_ACCESS_DENIED - Macie isn\'t allowed to access the object
    --     or the object is encrypted with a customer managed KMS key that
    --     Macie isn\'t allowed to use. Check the bucket policy and other
    --     permissions settings for the bucket and the object. If the object is
    --     encrypted, also ensure that it\'s encrypted with a key that Macie is
    --     allowed to use.
    --
    -- -   S3_OBJECT_EMPTY - Macie can retrieve the object but the object
    --     doesn\'t contain any content. Ensure that the object contains the
    --     correct entries. Also ensure that the list\'s criteria specify the
    --     correct bucket and object names.
    --
    -- -   S3_OBJECT_NOT_FOUND - The object doesn\'t exist in Amazon S3. Ensure
    --     that the list\'s criteria specify the correct bucket and object
    --     names.
    --
    -- -   S3_OBJECT_OVERSIZE - Macie can retrieve the object. However, the
    --     object contains too many entries or its storage size exceeds the
    --     quota for an allow list. Try breaking the list into multiple files
    --     and ensure that each file doesn\'t exceed any quotas. Then configure
    --     list settings in Macie for each file.
    --
    -- -   S3_THROTTLED - Amazon S3 throttled the request to retrieve the
    --     object. Wait a few minutes and then try again.
    --
    -- -   S3_USER_ACCESS_DENIED - Amazon S3 denied the request to retrieve the
    --     object. If the specified object exists, you\'re not allowed to
    --     access it or it\'s encrypted with an KMS key that you\'re not
    --     allowed to use. Work with your Amazon Web Services administrator to
    --     ensure that the list\'s criteria specify the correct bucket and
    --     object names, and you have read access to the bucket and the object.
    --     If the object is encrypted, also ensure that it\'s encrypted with a
    --     key that you\'re allowed to use.
    --
    -- -   UNKNOWN_ERROR - A transient or internal error occurred when Macie
    --     attempted to retrieve or parse the object. Wait a few minutes and
    --     then try again. A list can also have this status if it\'s encrypted
    --     with a key that Amazon S3 and Macie can\'t access or use.
    code :: AllowListStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowListStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'allowListStatus_description' - A brief description of the status of the allow list. Amazon Macie uses
-- this value to provide additional information about an error that
-- occurred when Macie tried to access and use the list\'s criteria.
--
-- 'code', 'allowListStatus_code' - The current status of the allow list. If the list\'s criteria specify a
-- regular expression (regex), this value is typically OK. Amazon Macie can
-- compile the expression.
--
-- If the list\'s criteria specify an S3 object, possible values are:
--
-- -   OK - Macie can retrieve and parse the contents of the object.
--
-- -   S3_OBJECT_ACCESS_DENIED - Macie isn\'t allowed to access the object
--     or the object is encrypted with a customer managed KMS key that
--     Macie isn\'t allowed to use. Check the bucket policy and other
--     permissions settings for the bucket and the object. If the object is
--     encrypted, also ensure that it\'s encrypted with a key that Macie is
--     allowed to use.
--
-- -   S3_OBJECT_EMPTY - Macie can retrieve the object but the object
--     doesn\'t contain any content. Ensure that the object contains the
--     correct entries. Also ensure that the list\'s criteria specify the
--     correct bucket and object names.
--
-- -   S3_OBJECT_NOT_FOUND - The object doesn\'t exist in Amazon S3. Ensure
--     that the list\'s criteria specify the correct bucket and object
--     names.
--
-- -   S3_OBJECT_OVERSIZE - Macie can retrieve the object. However, the
--     object contains too many entries or its storage size exceeds the
--     quota for an allow list. Try breaking the list into multiple files
--     and ensure that each file doesn\'t exceed any quotas. Then configure
--     list settings in Macie for each file.
--
-- -   S3_THROTTLED - Amazon S3 throttled the request to retrieve the
--     object. Wait a few minutes and then try again.
--
-- -   S3_USER_ACCESS_DENIED - Amazon S3 denied the request to retrieve the
--     object. If the specified object exists, you\'re not allowed to
--     access it or it\'s encrypted with an KMS key that you\'re not
--     allowed to use. Work with your Amazon Web Services administrator to
--     ensure that the list\'s criteria specify the correct bucket and
--     object names, and you have read access to the bucket and the object.
--     If the object is encrypted, also ensure that it\'s encrypted with a
--     key that you\'re allowed to use.
--
-- -   UNKNOWN_ERROR - A transient or internal error occurred when Macie
--     attempted to retrieve or parse the object. Wait a few minutes and
--     then try again. A list can also have this status if it\'s encrypted
--     with a key that Amazon S3 and Macie can\'t access or use.
newAllowListStatus ::
  -- | 'code'
  AllowListStatusCode ->
  AllowListStatus
newAllowListStatus pCode_ =
  AllowListStatus'
    { description = Prelude.Nothing,
      code = pCode_
    }

-- | A brief description of the status of the allow list. Amazon Macie uses
-- this value to provide additional information about an error that
-- occurred when Macie tried to access and use the list\'s criteria.
allowListStatus_description :: Lens.Lens' AllowListStatus (Prelude.Maybe Prelude.Text)
allowListStatus_description = Lens.lens (\AllowListStatus' {description} -> description) (\s@AllowListStatus' {} a -> s {description = a} :: AllowListStatus)

-- | The current status of the allow list. If the list\'s criteria specify a
-- regular expression (regex), this value is typically OK. Amazon Macie can
-- compile the expression.
--
-- If the list\'s criteria specify an S3 object, possible values are:
--
-- -   OK - Macie can retrieve and parse the contents of the object.
--
-- -   S3_OBJECT_ACCESS_DENIED - Macie isn\'t allowed to access the object
--     or the object is encrypted with a customer managed KMS key that
--     Macie isn\'t allowed to use. Check the bucket policy and other
--     permissions settings for the bucket and the object. If the object is
--     encrypted, also ensure that it\'s encrypted with a key that Macie is
--     allowed to use.
--
-- -   S3_OBJECT_EMPTY - Macie can retrieve the object but the object
--     doesn\'t contain any content. Ensure that the object contains the
--     correct entries. Also ensure that the list\'s criteria specify the
--     correct bucket and object names.
--
-- -   S3_OBJECT_NOT_FOUND - The object doesn\'t exist in Amazon S3. Ensure
--     that the list\'s criteria specify the correct bucket and object
--     names.
--
-- -   S3_OBJECT_OVERSIZE - Macie can retrieve the object. However, the
--     object contains too many entries or its storage size exceeds the
--     quota for an allow list. Try breaking the list into multiple files
--     and ensure that each file doesn\'t exceed any quotas. Then configure
--     list settings in Macie for each file.
--
-- -   S3_THROTTLED - Amazon S3 throttled the request to retrieve the
--     object. Wait a few minutes and then try again.
--
-- -   S3_USER_ACCESS_DENIED - Amazon S3 denied the request to retrieve the
--     object. If the specified object exists, you\'re not allowed to
--     access it or it\'s encrypted with an KMS key that you\'re not
--     allowed to use. Work with your Amazon Web Services administrator to
--     ensure that the list\'s criteria specify the correct bucket and
--     object names, and you have read access to the bucket and the object.
--     If the object is encrypted, also ensure that it\'s encrypted with a
--     key that you\'re allowed to use.
--
-- -   UNKNOWN_ERROR - A transient or internal error occurred when Macie
--     attempted to retrieve or parse the object. Wait a few minutes and
--     then try again. A list can also have this status if it\'s encrypted
--     with a key that Amazon S3 and Macie can\'t access or use.
allowListStatus_code :: Lens.Lens' AllowListStatus AllowListStatusCode
allowListStatus_code = Lens.lens (\AllowListStatus' {code} -> code) (\s@AllowListStatus' {} a -> s {code = a} :: AllowListStatus)

instance Data.FromJSON AllowListStatus where
  parseJSON =
    Data.withObject
      "AllowListStatus"
      ( \x ->
          AllowListStatus'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "code")
      )

instance Prelude.Hashable AllowListStatus where
  hashWithSalt _salt AllowListStatus' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` code

instance Prelude.NFData AllowListStatus where
  rnf AllowListStatus' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf code
