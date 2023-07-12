{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kinesis.ListStreamConsumers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the consumers registered to receive data from a stream using
-- enhanced fan-out, and provides information about each consumer.
--
-- This operation has a limit of 5 transactions per second per stream.
--
-- This operation returns paginated results.
module Amazonka.Kinesis.ListStreamConsumers
  ( -- * Creating a Request
    ListStreamConsumers (..),
    newListStreamConsumers,

    -- * Request Lenses
    listStreamConsumers_maxResults,
    listStreamConsumers_nextToken,
    listStreamConsumers_streamCreationTimestamp,
    listStreamConsumers_streamARN,

    -- * Destructuring the Response
    ListStreamConsumersResponse (..),
    newListStreamConsumersResponse,

    -- * Response Lenses
    listStreamConsumersResponse_consumers,
    listStreamConsumersResponse_nextToken,
    listStreamConsumersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStreamConsumers' smart constructor.
data ListStreamConsumers = ListStreamConsumers'
  { -- | The maximum number of consumers that you want a single call of
    -- @ListStreamConsumers@ to return. The default value is 100. If you
    -- specify a value greater than 100, at most 100 results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When the number of consumers that are registered with the data stream is
    -- greater than the default value for the @MaxResults@ parameter, or if you
    -- explicitly specify a value for @MaxResults@ that is less than the number
    -- of consumers that are registered with the data stream, the response
    -- includes a pagination token named @NextToken@. You can specify this
    -- @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list
    -- the next set of registered consumers.
    --
    -- Don\'t specify @StreamName@ or @StreamCreationTimestamp@ if you specify
    -- @NextToken@ because the latter unambiguously identifies the stream.
    --
    -- You can optionally specify a value for the @MaxResults@ parameter when
    -- you specify @NextToken@. If you specify a @MaxResults@ value that is
    -- less than the number of consumers that the operation returns if you
    -- don\'t specify @MaxResults@, the response will contain a new @NextToken@
    -- value. You can use the new @NextToken@ value in a subsequent call to the
    -- @ListStreamConsumers@ operation to list the next set of consumers.
    --
    -- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
    -- in the response to a call to @ListStreamConsumers@, you have 300 seconds
    -- to use that value. If you specify an expired token in a call to
    -- @ListStreamConsumers@, you get @ExpiredNextTokenException@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify this input parameter to distinguish data streams that have the
    -- same name. For example, if you create a data stream and then delete it,
    -- and you later create another data stream with the same name, you can use
    -- this input parameter to specify which of the two streams you want to
    -- list the consumers for.
    --
    -- You can\'t specify this parameter if you specify the NextToken
    -- parameter.
    streamCreationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the Kinesis data stream for which you want to list the
    -- registered consumers. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    streamARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamConsumers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStreamConsumers_maxResults' - The maximum number of consumers that you want a single call of
-- @ListStreamConsumers@ to return. The default value is 100. If you
-- specify a value greater than 100, at most 100 results are returned.
--
-- 'nextToken', 'listStreamConsumers_nextToken' - When the number of consumers that are registered with the data stream is
-- greater than the default value for the @MaxResults@ parameter, or if you
-- explicitly specify a value for @MaxResults@ that is less than the number
-- of consumers that are registered with the data stream, the response
-- includes a pagination token named @NextToken@. You can specify this
-- @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list
-- the next set of registered consumers.
--
-- Don\'t specify @StreamName@ or @StreamCreationTimestamp@ if you specify
-- @NextToken@ because the latter unambiguously identifies the stream.
--
-- You can optionally specify a value for the @MaxResults@ parameter when
-- you specify @NextToken@. If you specify a @MaxResults@ value that is
-- less than the number of consumers that the operation returns if you
-- don\'t specify @MaxResults@, the response will contain a new @NextToken@
-- value. You can use the new @NextToken@ value in a subsequent call to the
-- @ListStreamConsumers@ operation to list the next set of consumers.
--
-- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
-- in the response to a call to @ListStreamConsumers@, you have 300 seconds
-- to use that value. If you specify an expired token in a call to
-- @ListStreamConsumers@, you get @ExpiredNextTokenException@.
--
-- 'streamCreationTimestamp', 'listStreamConsumers_streamCreationTimestamp' - Specify this input parameter to distinguish data streams that have the
-- same name. For example, if you create a data stream and then delete it,
-- and you later create another data stream with the same name, you can use
-- this input parameter to specify which of the two streams you want to
-- list the consumers for.
--
-- You can\'t specify this parameter if you specify the NextToken
-- parameter.
--
-- 'streamARN', 'listStreamConsumers_streamARN' - The ARN of the Kinesis data stream for which you want to list the
-- registered consumers. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
newListStreamConsumers ::
  -- | 'streamARN'
  Prelude.Text ->
  ListStreamConsumers
newListStreamConsumers pStreamARN_ =
  ListStreamConsumers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      streamCreationTimestamp = Prelude.Nothing,
      streamARN = pStreamARN_
    }

-- | The maximum number of consumers that you want a single call of
-- @ListStreamConsumers@ to return. The default value is 100. If you
-- specify a value greater than 100, at most 100 results are returned.
listStreamConsumers_maxResults :: Lens.Lens' ListStreamConsumers (Prelude.Maybe Prelude.Natural)
listStreamConsumers_maxResults = Lens.lens (\ListStreamConsumers' {maxResults} -> maxResults) (\s@ListStreamConsumers' {} a -> s {maxResults = a} :: ListStreamConsumers)

-- | When the number of consumers that are registered with the data stream is
-- greater than the default value for the @MaxResults@ parameter, or if you
-- explicitly specify a value for @MaxResults@ that is less than the number
-- of consumers that are registered with the data stream, the response
-- includes a pagination token named @NextToken@. You can specify this
-- @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list
-- the next set of registered consumers.
--
-- Don\'t specify @StreamName@ or @StreamCreationTimestamp@ if you specify
-- @NextToken@ because the latter unambiguously identifies the stream.
--
-- You can optionally specify a value for the @MaxResults@ parameter when
-- you specify @NextToken@. If you specify a @MaxResults@ value that is
-- less than the number of consumers that the operation returns if you
-- don\'t specify @MaxResults@, the response will contain a new @NextToken@
-- value. You can use the new @NextToken@ value in a subsequent call to the
-- @ListStreamConsumers@ operation to list the next set of consumers.
--
-- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
-- in the response to a call to @ListStreamConsumers@, you have 300 seconds
-- to use that value. If you specify an expired token in a call to
-- @ListStreamConsumers@, you get @ExpiredNextTokenException@.
listStreamConsumers_nextToken :: Lens.Lens' ListStreamConsumers (Prelude.Maybe Prelude.Text)
listStreamConsumers_nextToken = Lens.lens (\ListStreamConsumers' {nextToken} -> nextToken) (\s@ListStreamConsumers' {} a -> s {nextToken = a} :: ListStreamConsumers)

-- | Specify this input parameter to distinguish data streams that have the
-- same name. For example, if you create a data stream and then delete it,
-- and you later create another data stream with the same name, you can use
-- this input parameter to specify which of the two streams you want to
-- list the consumers for.
--
-- You can\'t specify this parameter if you specify the NextToken
-- parameter.
listStreamConsumers_streamCreationTimestamp :: Lens.Lens' ListStreamConsumers (Prelude.Maybe Prelude.UTCTime)
listStreamConsumers_streamCreationTimestamp = Lens.lens (\ListStreamConsumers' {streamCreationTimestamp} -> streamCreationTimestamp) (\s@ListStreamConsumers' {} a -> s {streamCreationTimestamp = a} :: ListStreamConsumers) Prelude.. Lens.mapping Data._Time

-- | The ARN of the Kinesis data stream for which you want to list the
-- registered consumers. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
listStreamConsumers_streamARN :: Lens.Lens' ListStreamConsumers Prelude.Text
listStreamConsumers_streamARN = Lens.lens (\ListStreamConsumers' {streamARN} -> streamARN) (\s@ListStreamConsumers' {} a -> s {streamARN = a} :: ListStreamConsumers)

instance Core.AWSPager ListStreamConsumers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamConsumersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamConsumersResponse_consumers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listStreamConsumers_nextToken
          Lens..~ rs
          Lens.^? listStreamConsumersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListStreamConsumers where
  type
    AWSResponse ListStreamConsumers =
      ListStreamConsumersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamConsumersResponse'
            Prelude.<$> (x Data..?> "Consumers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreamConsumers where
  hashWithSalt _salt ListStreamConsumers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` streamCreationTimestamp
      `Prelude.hashWithSalt` streamARN

instance Prelude.NFData ListStreamConsumers where
  rnf ListStreamConsumers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streamCreationTimestamp
      `Prelude.seq` Prelude.rnf streamARN

instance Data.ToHeaders ListStreamConsumers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.ListStreamConsumers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStreamConsumers where
  toJSON ListStreamConsumers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StreamCreationTimestamp" Data..=)
              Prelude.<$> streamCreationTimestamp,
            Prelude.Just ("StreamARN" Data..= streamARN)
          ]
      )

instance Data.ToPath ListStreamConsumers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStreamConsumers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStreamConsumersResponse' smart constructor.
data ListStreamConsumersResponse = ListStreamConsumersResponse'
  { -- | An array of JSON objects. Each object represents one registered
    -- consumer.
    consumers :: Prelude.Maybe [Consumer],
    -- | When the number of consumers that are registered with the data stream is
    -- greater than the default value for the @MaxResults@ parameter, or if you
    -- explicitly specify a value for @MaxResults@ that is less than the number
    -- of registered consumers, the response includes a pagination token named
    -- @NextToken@. You can specify this @NextToken@ value in a subsequent call
    -- to @ListStreamConsumers@ to list the next set of registered consumers.
    -- For more information about the use of this pagination token when calling
    -- the @ListStreamConsumers@ operation, see
    -- ListStreamConsumersInput$NextToken.
    --
    -- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
    -- in the response to a call to @ListStreamConsumers@, you have 300 seconds
    -- to use that value. If you specify an expired token in a call to
    -- @ListStreamConsumers@, you get @ExpiredNextTokenException@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamConsumersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumers', 'listStreamConsumersResponse_consumers' - An array of JSON objects. Each object represents one registered
-- consumer.
--
-- 'nextToken', 'listStreamConsumersResponse_nextToken' - When the number of consumers that are registered with the data stream is
-- greater than the default value for the @MaxResults@ parameter, or if you
-- explicitly specify a value for @MaxResults@ that is less than the number
-- of registered consumers, the response includes a pagination token named
-- @NextToken@. You can specify this @NextToken@ value in a subsequent call
-- to @ListStreamConsumers@ to list the next set of registered consumers.
-- For more information about the use of this pagination token when calling
-- the @ListStreamConsumers@ operation, see
-- ListStreamConsumersInput$NextToken.
--
-- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
-- in the response to a call to @ListStreamConsumers@, you have 300 seconds
-- to use that value. If you specify an expired token in a call to
-- @ListStreamConsumers@, you get @ExpiredNextTokenException@.
--
-- 'httpStatus', 'listStreamConsumersResponse_httpStatus' - The response's http status code.
newListStreamConsumersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStreamConsumersResponse
newListStreamConsumersResponse pHttpStatus_ =
  ListStreamConsumersResponse'
    { consumers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of JSON objects. Each object represents one registered
-- consumer.
listStreamConsumersResponse_consumers :: Lens.Lens' ListStreamConsumersResponse (Prelude.Maybe [Consumer])
listStreamConsumersResponse_consumers = Lens.lens (\ListStreamConsumersResponse' {consumers} -> consumers) (\s@ListStreamConsumersResponse' {} a -> s {consumers = a} :: ListStreamConsumersResponse) Prelude.. Lens.mapping Lens.coerced

-- | When the number of consumers that are registered with the data stream is
-- greater than the default value for the @MaxResults@ parameter, or if you
-- explicitly specify a value for @MaxResults@ that is less than the number
-- of registered consumers, the response includes a pagination token named
-- @NextToken@. You can specify this @NextToken@ value in a subsequent call
-- to @ListStreamConsumers@ to list the next set of registered consumers.
-- For more information about the use of this pagination token when calling
-- the @ListStreamConsumers@ operation, see
-- ListStreamConsumersInput$NextToken.
--
-- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
-- in the response to a call to @ListStreamConsumers@, you have 300 seconds
-- to use that value. If you specify an expired token in a call to
-- @ListStreamConsumers@, you get @ExpiredNextTokenException@.
listStreamConsumersResponse_nextToken :: Lens.Lens' ListStreamConsumersResponse (Prelude.Maybe Prelude.Text)
listStreamConsumersResponse_nextToken = Lens.lens (\ListStreamConsumersResponse' {nextToken} -> nextToken) (\s@ListStreamConsumersResponse' {} a -> s {nextToken = a} :: ListStreamConsumersResponse)

-- | The response's http status code.
listStreamConsumersResponse_httpStatus :: Lens.Lens' ListStreamConsumersResponse Prelude.Int
listStreamConsumersResponse_httpStatus = Lens.lens (\ListStreamConsumersResponse' {httpStatus} -> httpStatus) (\s@ListStreamConsumersResponse' {} a -> s {httpStatus = a} :: ListStreamConsumersResponse)

instance Prelude.NFData ListStreamConsumersResponse where
  rnf ListStreamConsumersResponse' {..} =
    Prelude.rnf consumers
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
