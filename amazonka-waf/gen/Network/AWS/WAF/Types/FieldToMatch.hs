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
-- Module      : Network.AWS.WAF.Types.FieldToMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.FieldToMatch where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.MatchFieldType

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies where in a web request to look for @TargetString@.
--
-- /See:/ 'newFieldToMatch' smart constructor.
data FieldToMatch = FieldToMatch'
  { -- | When the value of @Type@ is @HEADER@, enter the name of the header that
    -- you want AWS WAF to search, for example, @User-Agent@ or @Referer@. The
    -- name of the header is not case sensitive.
    --
    -- When the value of @Type@ is @SINGLE_QUERY_ARG@, enter the name of the
    -- parameter that you want AWS WAF to search, for example, @UserName@ or
    -- @SalesRegion@. The parameter name is not case sensitive.
    --
    -- If the value of @Type@ is any other value, omit @Data@.
    data' :: Prelude.Maybe Prelude.Text,
    -- | The part of the web request that you want AWS WAF to search for a
    -- specified string. Parts of a request that you can search include the
    -- following:
    --
    -- -   @HEADER@: A specified request header, for example, the value of the
    --     @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the
    --     type, specify the name of the header in @Data@.
    --
    -- -   @METHOD@: The HTTP method, which indicated the type of operation
    --     that the request is asking the origin to perform. Amazon CloudFront
    --     supports the following methods: @DELETE@, @GET@, @HEAD@, @OPTIONS@,
    --     @PATCH@, @POST@, and @PUT@.
    --
    -- -   @QUERY_STRING@: A query string, which is the part of a URL that
    --     appears after a @?@ character, if any.
    --
    -- -   @URI@: The part of a web request that identifies a resource, for
    --     example, @\/images\/daily-ad.jpg@.
    --
    -- -   @BODY@: The part of a request that contains any additional data that
    --     you want to send to your web server as the HTTP request body, such
    --     as data from a form. The request body immediately follows the
    --     request headers. Note that only the first @8192@ bytes of the
    --     request body are forwarded to AWS WAF for inspection. To allow or
    --     block requests based on the length of the body, you can create a
    --     size constraint set. For more information, see
    --     CreateSizeConstraintSet.
    --
    -- -   @SINGLE_QUERY_ARG@: The parameter in the query string that you will
    --     inspect, such as /UserName/ or /SalesRegion/. The maximum length for
    --     @SINGLE_QUERY_ARG@ is 30 characters.
    --
    -- -   @ALL_QUERY_ARGS@: Similar to @SINGLE_QUERY_ARG@, but rather than
    --     inspecting a single parameter, AWS WAF will inspect all parameters
    --     within the query for the value or regex pattern that you specify in
    --     @TargetString@.
    type' :: MatchFieldType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FieldToMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'fieldToMatch_data' - When the value of @Type@ is @HEADER@, enter the name of the header that
-- you want AWS WAF to search, for example, @User-Agent@ or @Referer@. The
-- name of the header is not case sensitive.
--
-- When the value of @Type@ is @SINGLE_QUERY_ARG@, enter the name of the
-- parameter that you want AWS WAF to search, for example, @UserName@ or
-- @SalesRegion@. The parameter name is not case sensitive.
--
-- If the value of @Type@ is any other value, omit @Data@.
--
-- 'type'', 'fieldToMatch_type' - The part of the web request that you want AWS WAF to search for a
-- specified string. Parts of a request that you can search include the
-- following:
--
-- -   @HEADER@: A specified request header, for example, the value of the
--     @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the
--     type, specify the name of the header in @Data@.
--
-- -   @METHOD@: The HTTP method, which indicated the type of operation
--     that the request is asking the origin to perform. Amazon CloudFront
--     supports the following methods: @DELETE@, @GET@, @HEAD@, @OPTIONS@,
--     @PATCH@, @POST@, and @PUT@.
--
-- -   @QUERY_STRING@: A query string, which is the part of a URL that
--     appears after a @?@ character, if any.
--
-- -   @URI@: The part of a web request that identifies a resource, for
--     example, @\/images\/daily-ad.jpg@.
--
-- -   @BODY@: The part of a request that contains any additional data that
--     you want to send to your web server as the HTTP request body, such
--     as data from a form. The request body immediately follows the
--     request headers. Note that only the first @8192@ bytes of the
--     request body are forwarded to AWS WAF for inspection. To allow or
--     block requests based on the length of the body, you can create a
--     size constraint set. For more information, see
--     CreateSizeConstraintSet.
--
-- -   @SINGLE_QUERY_ARG@: The parameter in the query string that you will
--     inspect, such as /UserName/ or /SalesRegion/. The maximum length for
--     @SINGLE_QUERY_ARG@ is 30 characters.
--
-- -   @ALL_QUERY_ARGS@: Similar to @SINGLE_QUERY_ARG@, but rather than
--     inspecting a single parameter, AWS WAF will inspect all parameters
--     within the query for the value or regex pattern that you specify in
--     @TargetString@.
newFieldToMatch ::
  -- | 'type''
  MatchFieldType ->
  FieldToMatch
newFieldToMatch pType_ =
  FieldToMatch'
    { data' = Prelude.Nothing,
      type' = pType_
    }

-- | When the value of @Type@ is @HEADER@, enter the name of the header that
-- you want AWS WAF to search, for example, @User-Agent@ or @Referer@. The
-- name of the header is not case sensitive.
--
-- When the value of @Type@ is @SINGLE_QUERY_ARG@, enter the name of the
-- parameter that you want AWS WAF to search, for example, @UserName@ or
-- @SalesRegion@. The parameter name is not case sensitive.
--
-- If the value of @Type@ is any other value, omit @Data@.
fieldToMatch_data :: Lens.Lens' FieldToMatch (Prelude.Maybe Prelude.Text)
fieldToMatch_data = Lens.lens (\FieldToMatch' {data'} -> data') (\s@FieldToMatch' {} a -> s {data' = a} :: FieldToMatch)

-- | The part of the web request that you want AWS WAF to search for a
-- specified string. Parts of a request that you can search include the
-- following:
--
-- -   @HEADER@: A specified request header, for example, the value of the
--     @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the
--     type, specify the name of the header in @Data@.
--
-- -   @METHOD@: The HTTP method, which indicated the type of operation
--     that the request is asking the origin to perform. Amazon CloudFront
--     supports the following methods: @DELETE@, @GET@, @HEAD@, @OPTIONS@,
--     @PATCH@, @POST@, and @PUT@.
--
-- -   @QUERY_STRING@: A query string, which is the part of a URL that
--     appears after a @?@ character, if any.
--
-- -   @URI@: The part of a web request that identifies a resource, for
--     example, @\/images\/daily-ad.jpg@.
--
-- -   @BODY@: The part of a request that contains any additional data that
--     you want to send to your web server as the HTTP request body, such
--     as data from a form. The request body immediately follows the
--     request headers. Note that only the first @8192@ bytes of the
--     request body are forwarded to AWS WAF for inspection. To allow or
--     block requests based on the length of the body, you can create a
--     size constraint set. For more information, see
--     CreateSizeConstraintSet.
--
-- -   @SINGLE_QUERY_ARG@: The parameter in the query string that you will
--     inspect, such as /UserName/ or /SalesRegion/. The maximum length for
--     @SINGLE_QUERY_ARG@ is 30 characters.
--
-- -   @ALL_QUERY_ARGS@: Similar to @SINGLE_QUERY_ARG@, but rather than
--     inspecting a single parameter, AWS WAF will inspect all parameters
--     within the query for the value or regex pattern that you specify in
--     @TargetString@.
fieldToMatch_type :: Lens.Lens' FieldToMatch MatchFieldType
fieldToMatch_type = Lens.lens (\FieldToMatch' {type'} -> type') (\s@FieldToMatch' {} a -> s {type' = a} :: FieldToMatch)

instance Prelude.FromJSON FieldToMatch where
  parseJSON =
    Prelude.withObject
      "FieldToMatch"
      ( \x ->
          FieldToMatch'
            Prelude.<$> (x Prelude..:? "Data")
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable FieldToMatch

instance Prelude.NFData FieldToMatch

instance Prelude.ToJSON FieldToMatch where
  toJSON FieldToMatch' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Data" Prelude..=) Prelude.<$> data',
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
